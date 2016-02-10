#!/usr/bin/env python

import os
import sys
import time
from subprocess import call
from continuous_file_executor import *

def execute_command(command, exec_state, output):
  try:
    command_output = os.popen(command)
    last_char = command_output.read(1)
    while last_char != '':
      output.write(last_char)
      last_char = command_output.read(1)
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

if __name__ == '__main__':
  script_name = sys.argv[0]
  if len(sys.argv) < 3:
    print "Usage: " + sys.argv[0] + " <filename> <command> <arg1> <arg2> ... <argN>"
    exit(1)

  filename = sys.argv[1]
  command = ' '.join(sys.argv[2:len(sys.argv)])

  main(filename, command)
