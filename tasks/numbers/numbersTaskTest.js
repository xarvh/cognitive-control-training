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


    var fmtTime = _.identity;


    lab.test('accepts an initial isi', (done) => {

        var tellDigitTimestamps = [];
        var onCount = sinon.spy();

        var task = new NumbersTask({
            formatTimestamp: (a) => a,
            onCount: onCount,
            emitDigit: () => {
                tellDigitTimestamps.push(new Date);
                if (tellDigitTimestamps.length > 4) {
                    task.stop();
                }
            },
        });

        var isi = 25;
        var initialDelay = 10;
        task.start(isi, initialDelay);

        timeoutSet(initialDelay / 2, function () {
            assert.throws(() => task.start(isi, initialDelay), /already/);
        });

        timeoutSet(initialDelay + isi * 8, function () {
            assert.equal(tellDigitTimestamps.length, 5);

            tellDigitTimestamps.reduce((previous, current) => {
                var time = current - previous;
                assert(time > isi / 2);
                assert(time < isi * 2);
                return current;
            });

            // 5 digits are called, but the first one is not counted, and the
            // game is terminated before the last one can register as 'miss'.
            assert.deepEqual(onCount.args, [ ['miss', 25], ['miss', 25], ['miss', 25] ]);

            // test aggregate data
            var aggregate = task.getAggregateEventsTable(fmtTime);
            assert.equal(aggregate.length, 2)

            var session = _.zipObject(aggregate[0], aggregate[1]);
            assert.equal(session['Right'], 0);
            assert.equal(session['Wrong'], 0);
            assert.equal(session['Miss'], 3);
            assert.equal(session['Accuracy (normalized)'], 0);
            assert.equal(session['Starting ISI'], 25);
            assert.equal(session['Max ISI'], 25);
            assert.equal(session['Min ISI'], 25);
            assert.equal(session['Trials at minimum ISI'], 3);

            // test events
            assert.deepEqual(_.map(task.getEventsTable(fmtTime), 2), ['Event', 'start', 'miss', 'miss', 'miss', 'stop']);

            done();
        });
    });


    lab.test('correctly counts trials', (done) => {

        var onCount = sinon.spy();

        var task;
        var digits = []
        var emitDigit = (n) => setImmediate(function () {
            switch (digits.length) {
                case 0: break;
                case 1: task.setUserAnswer(20); break; // wrong
                case 2: break; // miss
                case 3: task.setUserAnswer(_.last(digits) + n); break; // right
                case 4: task.stop(); break;
            }

            digits.push(n);
        });

        task = new NumbersTask({
            formatTimestamp: (a) => a,
            onCount: onCount,
            emitDigit: emitDigit,
        });

        task.start(25, 25);
        timeoutSet(25 * 6, function () {
            assert.deepEqual(onCount.args, [ ['wrong', 25], ['miss', 25], ['right', 25] ]);

            var aggregate = task.getAggregateEventsTable(fmtTime);
            var session = _.zipObject(aggregate[0], aggregate[1]);
            assert.equal(session['Right'], 1);
            assert.equal(session['Wrong'], 1);
            assert.equal(session['Miss'], 1);
            assert.equal(session['Accuracy (normalized)'], 1/3);

            assert.deepEqual(_.map(task.getEventsTable(fmtTime), 2), ['Event', 'start', 'wrong', 'miss', 'right', 'stop']);
            done();
        });
    });


    lab.test('ignores answers given after task has been stopped', (done) => {
        var count = 0;
        var task = new NumbersTask({
            formatTimestamp: (a) => a,
            onCount: _.noop,
            emitDigit: () => {
                count++;
                if (count < 4) return;
                task.stop();
                setImmediate(continueTest);
            }
        });

        task.start(10, 0);
        function continueTest() {
            task.setUserAnswer(20);
            assert.equal(_.last(task.getEvents()).name, 'stop');
            done();
        }

    });

    lab.test('adjusts the difficulty to the user performance');
});
