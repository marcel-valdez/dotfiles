#!/usr/bin/env python

import sys
import time

from os import popen3
from continuous_file_executor import watch

def execute_command(command, exec_state, output):
  try:
    (_, cmd_stdout, cmd_stderror) = popen3(command)
    last_char = cmd_stdout.read(1)
    while last_char != '':
      output.write(last_char)
      last_char = cmd_stdout.read(1)

    last_char = cmd_stderror.read(1)
    while last_char != '':
      output.write(last_char)
      last_char = cmd_stderror.read(1)

  except Exception, e:
    print "Error while executing command: " + ' '.join(map(str, command))
    print "Details: " + str(e)
  except SystemExit, se:
    print "-- Exit Status: " + str(se)

  print "-- Done."
  time.sleep(0.1)
  exec_state['done'] = True

def main(_filename, _command):
  watch(_filename, execute_command, _command)

def quote_when_spaced(arg):
  if ' ' in arg:
    return '"' + arg + '"'

  return arg

def not_empty(arg):
  return arg != ''

if __name__ == '__main__':
  SCRIPT_NAME = sys.argv[0]
  if len(sys.argv) < 3:
    print "Usage: " + sys.argv[0] + " <filename> <command> <arg1> <arg2> ... <argN>"
    exit(1)

  FILENAME = sys.argv[1]
  COMMAND_ARGS = sys.argv[2:len(sys.argv)]
  COMMAND = ' '.join(map(quote_when_spaced, filter(not_empty, COMMAND_ARGS)))

  main(FILENAME, COMMAND)
