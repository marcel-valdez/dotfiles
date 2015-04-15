#!/usr/bin/env python

from cStringIO import StringIO
from thread import *
import os
import sys
import time
import curses

def validate_file(filename):
  if not os.path.isfile(filename):
    print "The file " + filename + " does not exist."
    exit(1)
  if not os.access(filename, os.R_OK):
    print "The file " + filename + " is not accessible."
    exit(1)

def get_modification_time(filename):
  return os.path.getmtime(filename)

def init_curses():
  win = curses.initscr()
  curses.noecho()
  curses.curs_set(0)
  return win

def clear_screen(win):
  win.clear()
  win.refresh()

def end_screen():
  curses.endwin()

def get_new_content(previous_content, current_content):
  return current_content[len(previous_content):]

def print_output(win, output, exec_state):
  previous_content = ""
  while not exec_state['done']:
    contents = output.getvalue()
    if contents != None:
      new_content = get_new_content(previous_content, contents)
      if len(new_content) > 0:
        win.addstr(new_content)
        win.refresh()
    previous_content = contents
    time.sleep(0.01) # avoid overloading the CPU
  output.close()

def replace_stdout(new_stdout):
  orig = sys.stdout
  sys.stdout = new_stdout
  return orig

def execute_file(filename, exec_state):
  print "-- Change detected, executing file: " + filename + " --"
  try:
    execfile(filename, { '__name__' : '__main__' })
  except Exception, ex:
    print "Error occurred while executing " + filename
    print str(ex)
  print "-- Done. --"
  time.sleep(0.1) # give print_output time to print remaining contents
  exec_state['done'] = True

def main(filename):
  validate_file(filename)
  last_modification_time = get_modification_time(filename)
  win = init_curses()
  try:
    while True:
      if last_modification_time != get_modification_time(filename):
        exec_state = { 'done': False }
        clear_screen(win)
        proc_stdout = StringIO()
        replace_stdout(proc_stdout)
        start_new_thread(execute_file, (filename, exec_state))
        last_modification_time = get_modification_time(filename)
        print_output(win, proc_stdout, exec_state)
      time.sleep(0.5)
  finally:
    end_screen()

if __name__ == '__main__':
  if len(sys.argv) < 2:
    print "Usage: " + sys.argv[0] + " <filename> "
    exit(1)

  main(sys.argv[1])
