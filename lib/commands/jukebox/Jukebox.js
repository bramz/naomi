import child_process from 'child_process'
import fs from 'fs'
import uuidv4 from 'uuid/v4'

class Jukebox {
    constructor() {
        this.volume = 0.5
        this.playing = false
        this.dispatcher = null
        this.queue = []
        this.nowPlaying = null
        this.playlist = null

        this.start = this.start.bind(this)
        this.stop = this.stop.bind(this)
        this.next = this.next.bind(this)
        this.fetch = this.fetch.bind(this)
        this.search = this.search.bind(this)
        this.play = this.play.bind(this)
        this.whatsPlaying = this.whatsPlaying.bind(this)
        this.setVolume = this.setVolume.bind(this)
        this.remove = this.remove.bind(this)
        this.rename = this.rename.bind(this)
        this.queueSong = this.queueSong.bind(this)
        this.imFeelingLucky = this.imFeelingLucky.bind(this)
        this.setPlaylist = this.setPlaylist.bind(this)
        this.insertIntoPlaylist = this.insertIntoPlaylist.bind(this)

        this._downloadYtSong = this._downloadYtSong.bind(this)
        this._playSong = this._playSong.bind(this)
    }

    start(context) {
        return new Promise((resolve, _reject) => {
            if (this.playing) {
                resolve()
            } else {
                const { message, logger, state } = context

                if (message.member === null) {
                    message.reply('you don\'t seem to be in a voice channel.')
                    return
                }

                const { voiceChannel } = message.member

                if (state.voiceConnection) {
                    this.next(context)
                } else {
                    voiceChannel.join()
                        .then(connection => {
                            logger.info(`Connected to voice channel ${voiceChannel}`)

                            state.voiceConnection = connection
                            this.next(context)
                        })
                }
            }
        })
    }

    stop({ client }) {
        if (this.playing) {
            this.playing = false
            this.nowPlaying = null

            client.user.setActivity(null)
            this.dispatcher.end()
        }
    }

    next(context) {
        const { logger, message, state } = context

        if (!state.voiceConnection) {
            this.start(context)
                .then(() => this.next(context))
            return
        }

        if (this.dispatcher !== null) {
            this.dispatcher.end()
            return
        }

        this.playing = true
        this.nextSong(context)
            .then(song => {
                logger.info(`Playing ${song.path}...`)

                fs.access(song.path, (err) => {
                    if (err) {
                        message.reply(`the song file for ${song.title} does not exist. I am fetching it...`)

                        this._downloadYtSong(context, song.url, song.path)
                            .then(() => {
                                message.reply(`I fetched the song file for ${song.title}! Playing it now...`)

                                this._playSong(context, song)
                            }, (error) => {
                                logger.error(error)
                                message.reply('I was unable to fetch that song.')
                            })
                    } else {
                        this._playSong(context, song)
                    }
                })
            }, () => {
                message.reply('An error occurred when playing next song.')
            })
    }

    nextSong({ logger, db }) {
        const { playlist } = this

        return this.queue.length > 0
            ? Promise.resolve(this.queue.shift())
            : new Promise((resolve, reject) => {
                const handle = (err, row) => {
                    if (err) {
                        logger.error(`Failed to fetch next song: ${err.toString()}`)
                        reject(err)
                    } else if (!row) {
                        reject('There are no songs')
                    } else {
                        resolve(row)
                    }
                }

                playlist === null
                    ? db.get('SELECT * FROM jukebox_songs WHERE deleted = 0 ORDER BY RANDOM() LIMIT 1', handle)
                    : db.get('SELECT * FROM jukebox_songs WHERE deleted = 0 AND id = (SELECT song_id FROM jukebox_playlists WHERE playlist_name = ? ORDER BY RANDOM() LIMIT 1)', [playlist], handle)
            })
    }

    fetch(context, url) {
        const { db, logger, message, config } = context
        const outputPath = `${config.jukeboxDirectory}/${uuidv4()}.opus`

        this._downloadYtSong(context, url, outputPath)
            .then(() => {
                const titleProc = child_process.spawn(
                    'youtube-dl',
                    ['--get-title', url]
                )

                titleProc.stdout.on('data', title => {
                    const stmt = db.prepare('INSERT INTO jukebox_songs (url, title, path, author) VALUES (?, ?, ?, ?)')

                    stmt.run(
                        url,
                        title.toString().trim(),
                        outputPath,
                        `${message.author.username}#${message.author.discriminator}`
                        , function(err) {
                            if (err) {
                                logger.error(`Failed to insert jukebox song: ${err}`)

                                if (/UNIQUE constraint failed: jukebox_songs\.url/.test(err.toString())) {
                                    message.reply('I already have that song.')
                                } else {
                                    message.reply('there was an error fetching the file.')
                                }
                            } else {
                                message.reply(`the download for <${url}> (#${this.lastID}) completed.`)
                            }
                        })
                })

                titleProc.on('close', code => {
                    if (code !== 0) {
                        logger.error(`Title fetch for ${url} failed with exit code ${code}`)
                        message.reply('there was an error fetching the file.')
                    }
                })
            }, (error) => {
                logger.error(error)
                message.reply('there was an error fetching the file.')
            })
    }

    search({ db, logger, message }, query) {
        db.all('SELECT * FROM jukebox_songs WHERE deleted = 0 AND UPPER(title) LIKE UPPER(?) ORDER by ID ASC', [`%${query}%`], (err, rows) => {
            if (err) {
                logger.error(err.toString())
                message.reply('There was an error performing the search.')
            } else if (rows.length === 0) {
                message.reply('I could not find any matches.')
            } else {
                const titles = rows.map(row => `(${row.id}) ${row.title}`).slice(0, 5).join('\n')
                const andMore = rows.length > 5 ? `\n ... and ${rows.length - 5} more results.` : ''
                message.reply('I found the following matches:\n```' + titles + '```' + andMore)
            }
        })
    }

    play(context, id) {
        const { db, logger, message } = context

        db.get('SELECT * FROM jukebox_songs WHERE id = ? AND deleted = 0', [id], (err, row) => {
            if (err) {
                logger.error(err.toString())
                message.reply('could not find a song with that ID.')
            } else {
                this.queue.unshift(row)
                this.next(context)
            }
        })
    }

    whatsPlaying({ message }) {
        if (this.nowPlaying) {
            message.reply(`${this.nowPlaying.title} is currently playing.`)
        } else {
            message.reply('no song is currently playing.')
        }
    }

    setVolume({ message }, volume) {
        const oldVolume = Math.floor(this.volume * 100)
        const adjustedVolume = parseInt(volume, 10) / 100

        this.volume = adjustedVolume
        message.reply(`I adjusted the volume to ${volume}. It was previously set to ${oldVolume}.`)
    }

    setPlaylist({ message }, name) {
        this.playlist = name === 'unset' ? null : name
        message.reply(
            name === 'unset'
                ? 'the current playlist has been unset.'
                : `the playlist has been set to ${name}.`)
    }

    insertIntoPlaylist({ db, message, logger }, data) {
        const [ playlist, id ] = data.split(' ')

        db.get('SELECT id FROM jukebox_songs WHERE id = ?', [id], (err, row) => {
            if (err || !row) {
                if (err) {
                    logger.error(err.toString())
                }

                message.reply('I could not find a song with that ID.')
                return
            }

            const { id } = row
            db.run('INSERT INTO jukebox_playlists (song_id, playlist_name) VALUES (?, ?)', [id, playlist], err => {
                if (err) {
                    logger.error(err.toString())
                    message.reply('I could not add the song to the playlist.')
                } else {
                    message.reply('the song has been added.')
                }
            })
        })
    }

    remove({ db, logger, message }, id) {
        db.run('UPDATE jukebox_songs SET deleted = 1 WHERE id = ? AND deleted = 0', [id], err => {
            if (err) {
                logger.error(err.toString())
                message.reply('could not find a song with that ID.')
            } else {
                message.reply('the song with that ID has been removed.')
            }
        })
    }

    rename({ client, db, message, logger }, data) {
        const match = data.match(/^([0-9]+) (.*)$/)

        if (match === null) {
            message.reply('you have provided an invalid format.')
            return
        }

        const [ _, id, title ] = match
        logger.info(`Updating song ${id} title to "${title}"...`)

        db.run('UPDATE jukebox_songs SET title = ? WHERE id = ? AND deleted = 0', [ title, id ], err => {
            if (err) {
                message.reply('could not find a song with that ID.')
            } else {
                message.reply('the song has been renamed.')
            }
        })

        if (this.nowPlaying && this.nowPlaying.id === parseInt(id, 10)) {
            this.nowPlaying.title = title

            client.user.setActivity(this.nowPlaying.title)
        }
    }

    queueSong({ db, logger, message }, id) {
        db.get('SELECT * FROM jukebox_songs WHERE id = ? AND deleted = 0', [id], (err, row) => {
            if (err) {
                logger.error(err.toString())
                message.reply('could not find a song with that ID.')
            } else {
                message.reply(`song ${id} has been added to the play queue.`)
                this.queue.push(row)
            }
        })
    }

    imFeelingLucky(context, query) {
        const { db, logger, message } = context

        db.all('SELECT * FROM jukebox_songs WHERE deleted = 0 AND UPPER(title) LIKE UPPER(?) ORDER by ID ASC', [`%${query}%`], (err, rows) => {
            if (err) {
                logger.error(err.toString())
                message.reply('There was an error performing the search.')
            } else if (rows.length === 0) {
                message.reply('I could not find any matches.')
            } else {
                this.queue.unshift(rows[0])
                this.next(context)
            }
        })
    }

    _downloadYtSong({ logger }, url, outputPath) {
        return new Promise((resolve, reject) => {
            logger.info(`Fetching ${url} to ${outputPath}...`)

            const proc = child_process.spawn(
                'youtube-dl',
                [
                    '--output',
                    outputPath,
                    '--audio-format',
                    'opus',
                    '--audio-quality',
                    '9',
                    '-x',
                    url
                ]
            )

            proc.stderr.on('data', data => logger.error(data.toString()))
            proc.on('close', code => {
                if (code === 0) {
                    resolve(null)
                } else {
                    reject(`Fetch of ${url} failed with exit code ${code}`)
                }
            })
        })
    }

    _playSong(context, song) {
        const { client, state } = context

        this.nowPlaying = song
        client.user.setActivity(this.nowPlaying.title)

        this.dispatcher = state.voiceConnection.playFile(song.path, {
            volume: this.volume,
            bitrate: 12000
        })

        this.dispatcher.on('end', () => {
            this.dispatcher = null

            if (this.playing) {
                this.next(context)
            }
        })
    }
}

module.exports = Jukebox
