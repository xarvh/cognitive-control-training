'use strict';

const lab = exports.lab = require('lab').script();

const _ = require('lodash');
const assert = require('assert');
const sinon = require('sinon');


const NumbersTask = require('./numbersTask');


// syntactic sugar
function timeoutSet(time, f) { setTimeout(f, time); }


lab.experiment('Numbers Task', () => {

    lab.before((done) => { global._ = _; done(); });
    lab.after((done) => { delete global._; done(); });



    lab.test('accepts an initial inverse speed', (done) => {

        var tellNumberTimestamps = [];

        var task = new NumbersTask({
            formatTimestamp: (a) => a,
            onCount: sinon.spy(),
            tellNumber: () => {
                tellNumberTimestamps.push(new Date);
                if (tellNumberTimestamps.length > 4) {
                    task.stop();
                }
            },
        });

        var invSpeed = 25;
        var initialDelay = 10;
        task.start(invSpeed, initialDelay);

        timeoutSet(initialDelay / 2, function () {
            assert.throws(() => task.start(invSpeed, initialDelay), /already/);
        });

        timeoutSet(initialDelay + invSpeed * 8, function () {
            assert.equal(tellNumberTimestamps.length, 5);

            tellNumberTimestamps.reduce((previous, current) => {
                var time = current - previous;
                assert(time > invSpeed / 2);
                assert(time < invSpeed * 2);
                return current;
            });

            done();
        });
    });
});
