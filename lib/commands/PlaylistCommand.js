import Command from './Command'
import PromisedDatabase from '../util/PromisedDatabase'

const list = ({ db, logger, message }) => {
    db.all('SELECT DISTINCT playlist_name FROM jukebox_playlists', (err, rows) => {
        if (err) {
            logger.error(err.toString())
            message.reply('I don\'t know what happened.')
            return
        } else if (!rows.length) {
            message.reply('there are no playlists.')
            return
        }

        const playlists = rows.map(({ playlist_name }) => `\`${playlist_name}\``).join(', ')
        message.reply(`the playlists I know about are ${playlists}`)
    })
}

const remove = ({ db, logger, message }, playlist) => {
    const pdb = new PromisedDatabase(db)

    pdb.run('DELETE FROM jukebox_playlists WHERE playlist_name = ?', [playlist])
        .then(() => message.reply(`the playlist '${playlist}' has been removed.`))
        .catch(err => {
            logger.error(err.toString())
            message.reply('I was unable to remove that playlist.')
        })
}

const show = ({ db, logger, message }, playlist) => {
    db.all('SELECT song.id, song.title FROM jukebox_playlists playlist INNER JOIN jukebox_songs song ON playlist.song_id = song.id WHERE playlist.playlist_name = ?', [playlist], (err, rows) => {
        if (err) {
            logger.error(err.toString())
            message.reply('I had trouble fetching that playlist.')
        } else if (!rows || !rows.length) {
            message.reply('I had trouble finding that playlist.')
        }

        const names = rows
            .map(({ id, title }) => `* (${id}) ${title}`)
            .join('\n')

        message.reply('I found the following songs: ```\n' + names + '```')
    })
}

module.exports = new Command({
    name: 'playlist',
    command: list,
    subcommands: {
        list,
        remove,
        show
    }
})
