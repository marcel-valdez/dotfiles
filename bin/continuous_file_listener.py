#!/usr/bin/env python

import os
import sys
import time
from subprocess import call
from continuous_file_executor import *

def execute_command(command, exec_state, output):
  try:
    (cmd_stdin, cmd_stdout, cmd_stderror) = os.popen3(command)
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
  except SystemExit, exit:
    print "-- Exit Status: " + str(exit)

  print "-- Done."
  time.sleep(0.1)
  exec_state['done'] = True

def main(filename, command):
  watch(filename, execute_command, command)

def quoteWhenSpaced(arg):
  if ' ' in arg:
    return '"' + arg + '"'
  else:
    return arg

def notEmpty(arg):
  return arg != ''

if __name__ == '__main__':
  script_name = sys.argv[0]
  if len(sys.argv) < 3:
    print "Usage: " + sys.argv[0] + " <filename> <command> <arg1> <arg2> ... <argN>"
    sys.exit(1)

  filename = sys.argv[1]
  command_args = sys.argv[2:len(sys.argv)]
  command = ' '.join(map(quoteWhenSpaced, filter(notEmpty, command_args)))

  main(filename, command)
