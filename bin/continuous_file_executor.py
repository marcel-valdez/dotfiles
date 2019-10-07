#!/usr/bin/env python

from cStringIO import StringIO
from thread import *
import os
import sys
import time
import curses

PAD_LINES = 9000
PAD_ROWS = 1000

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
  curses.initscr()
  win = curses.newpad(PAD_LINES, PAD_ROWS)
  win.idlok(1)
  win.scrollok(True)
  win.setscrreg(0, PAD_LINES - 1)
  win.nodelay(True)

  curses.noecho()
  curses.curs_set(0)
  curses.curs_set(1)
  curses.use_default_colors()
  return win

def clear_screen(win):
  win.clear()
  refresh_screen(win)

def end_screen():
  curses.endwin()

def refresh_screen(win):
  win.refresh(0, 0, 0, 0, curses.LINES - 1, curses.COLS - 1)

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
        refresh_screen(win)
    previous_content = contents
    time.sleep(0.01) # avoid overloading the CPU

def replace_stdout(new_stdout):
  orig = sys.stdout
  sys.stdout = new_stdout
  return orig

def execute_file(filename, exec_state, output):
  try:
    execfile(filename, { '__name__' : '__main__' })
  except Exception, ex:
    print "Error occurred while executing " + filename
    print str(ex)
  except SystemExit, exit:
    print "-- Exit status: " + str(exit)

  print "-- Done."
  time.sleep(0.1) # give print_output time to print remaining contents
  exec_state['done'] = True

def print_mod_message(filename, modtime):
  modtime_msg = time.strftime("%I:%M:%S %p", time.localtime(modtime))
  print "-- [" + modtime_msg + "] Change detected."

def handle_scroll(win):
  key = win.getch()
  if key == ord('n'):
      win.scroll(-1)
      win.redrawwin()
      refresh_screen(win)
# TODO: scrolling backwards deletes content.
#  elif key == ord('p'):
#      win.redrawwin()
#      win.scroll(-1)
#      refresh_screen(win)

def handle_modification(win, filename, modtime, parameter):
  exec_state = { 'done': False }
  clear_screen(win)
  refresh_screen(win)
  proc_stdout = StringIO()
  replace_stdout(proc_stdout)
  print_mod_message(filename, modtime)
  start_new_thread(method, (parameter, exec_state, proc_stdout))
  print_output(win, proc_stdout, exec_state)
  proc_stdout.close()

def watch(filename, method, parameter):
  validate_file(filename)
  last_modification_time = get_modification_time(filename)
  win = init_curses()
  try:
    while True:
      modtime = get_modification_time(filename)
      if last_modification_time != modtime
        handle_modification(win, filename, modtime, parameter)
        last_modification_time = modtime
      else:
        handle_scroll(win, proc_stdout)

      time.sleep(0.1)
  finally:
    end_screen()

def main(filename):
  watch(filename, execute_file, filename)

if __name__ == '__main__':
  if len(sys.argv) < 2:
    print "Usage: " + sys.argv[0] + " <filename> "
    exit(1)

  main(sys.argv[1])
