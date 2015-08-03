/* global exports */
"use strict";

// module Test.Utils

exports.exit = function (exitCode) {
  return function () {
    process.exit(exitCode);
  };
};

var globalExitStatus = 0;

exports.setExitStatus = function (exitCode) {
  return function () {
    globalExitStatus = exitCode;
  };
};

exports.getExitStatus = function () {
  return globalExitStatus;
};
