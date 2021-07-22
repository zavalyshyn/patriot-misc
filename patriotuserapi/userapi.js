const express = require('express');
const path = require('path');
const cookieParser = require('cookie-parser');
const createError = require('http-errors');
// const https = require('https');
const http = require('http');
const cors = require('cors');
const fs = require('fs');

const indexRouter = require('./routes/index');
const policiesRouter = require('./routes/policies');
const devicesRouter = require('./routes/devices');
const usersRouter = require('./routes/users');

const app = express();

// const options = {
//   key: fs.readFileSync(`${__dirname}/keys/server-key.pem`),
//   cert: fs.readFileSync(`${__dirname}/keys/server-cert.pem`)
// };

app.use(cors());
app.use(express.json());
app.use(express.urlencoded({ extended: false }));
app.use(cookieParser());

app.use('/api', indexRouter);
app.use('/api/policies', policiesRouter);
app.use('/api/devices', devicesRouter);
app.use('/api/users', usersRouter);

// error handler
app.use(function(req, res) {
    res.status(404);
    res.send("Wrong API call.")
});

// Start HTTP Server
var port = 4005;
app.set('port', port);

// var server = https.createServer(options,app);
var server = http.createServer(app);
server.listen(port);
server.on('error', onError);
server.on('listening', onListening);

/**
 * Event listener for HTTP server "error" event.
 */

function onError(error) {
    if (error.syscall !== 'listen') {
        throw error;
    }

    var bind = typeof port === 'string'
        ? 'Pipe ' + port
        : 'Port ' + port;

    // handle specific listen errors with friendly messages
    switch (error.code) {
        case 'EACCES':
            console.error(bind + ' requires elevated privileges');
            process.exit(1);
            break;
        case 'EADDRINUSE':
            console.error(bind + ' is already in use');
            process.exit(1);
            break;
        default:
            throw error;
    }
}

/**
 * Event listener for HTTP server "listening" event.
 */

function onListening() {
    var addr = server.address();
    var bind = typeof addr === 'string'
        ? 'pipe ' + addr
        : 'port ' + addr.port;
    console.log('Listening on ' + bind);
}
