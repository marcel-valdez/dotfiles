#!/usr/bin/env python

import sys
import time
from subprocess import run
from continuous_file_executor import *

def execute_command(command, exec_state, output):
  try:
    if command is str:
      command = command.split(' ', encoding='utf-8')
    process = run(command)
    cmd_stdout = process.stdout
    cmd_stderr = process.stderr

    def print_std(std):
      if std is str:
        print(std)
      else:
        last_char = std.read(1)
        while last_char != '':
          output.write(last_char)
          last_char = std.read(1)

    if process.stdout:
      print_std(process.stdout)

    if process.stderr:
      print_std(process.stderr)
  except Exception as e:
    print("Error while executing command: " + ' '.join(map(str, command)))
    print("Details: " + str(e))
  except SystemExit as se:
    print("-- Exit Status: " + str(se))

  print("-- Done.")
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
    print("Usage: " + sys.argv[0] + " <filename> <command> <arg1> <arg2> ... <argN>")
    sys.exit(1)

  filename = sys.argv[1]
  command_args = sys.argv[2:len(sys.argv)]
  command = ' '.join(map(quoteWhenSpaced, filter(notEmpty, command_args)))

  main(filename, command)
