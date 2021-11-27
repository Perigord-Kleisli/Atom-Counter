#!/usr/bin/python

import curses
import os
import pyperclip3


def main(stdscr):
    """ main """
    stdscr.clear()
    stdscr.addstr(0, 0, "Formula: ")
    stdscr.refresh()
    curses.echo(True)
    formula = stdscr.getstr(1, 0).decode(encoding="utf-8")
    stdscr.clear()
    coef_list = [""]
    old_coef = False

    while True:
        stdscr.clear()
        stdscr.addstr(0, 0, formula, curses.A_BOLD)
        stdscr.addstr(1, 0, "-------", curses.A_BOLD)
        stdscr.addstr(2, 0, "Coefficients: ")
        stdscr.refresh()

        curses.echo(True)

        if not old_coef:
            coef_list.append(str(stdscr.getstr(3, 0)).strip("'b"))
            old_coef = False

        command = (f'./Atom-Counter -t "{formula} {coef_list[-1]}"')
        table = os.popen(command).read()
        stdscr.addstr(4, 0, table)
        stdscr.addstr(4, 40, "press:", curses.A_DIM)
        stdscr.addstr(5, 45, "q to quit", curses.A_DIM)
        stdscr.addstr(6, 45, "r to retype formula", curses.A_DIM)
        stdscr.addstr(7, 45, f'<UP> to use the last coefficients: {coef_list[-2]} ', curses.A_DIM)
        stdscr.addstr(8, 45, "Any other key to type another coefficient", curses.A_DIM)
        pyperclip3.copy(table)

        after = stdscr.getch()
        stdscr.keypad(True)
        stdscr.refresh()
        if after == ord('q'):
            break

        if after == ord('r'):
            stdscr.clear()
            stdscr.addstr(0, 0, "Formula: ")
            stdscr.refresh()
            formula = stdscr.getstr(1, 0)
            stdscr.clear()
        elif after == curses.KEY_UP:
            while after == curses.KEY_UP and len(coef_list) > 1:
                stdscr.clear()
                stdscr.addstr(0, 0, formula, curses.A_BOLD)
                stdscr.addstr(1, 0, f'------- History items remaining: {len(coef_list)}', curses.A_BOLD)
                stdscr.addstr(2, 0, "Coefficients: ")
                stdscr.addstr(3, 0, coef_list.pop())
                pyperclip3.copy(table)
                stdscr.refresh()
                after = stdscr.getch()
            old_coef = True
            if len(coef_list) == 1:
                stdscr.addstr(1, 0, "------- No more items in history", curses.A_BOLD)
                stdscr.refresh()
                stdscr.getch()
                old_coef = False

curses.wrapper(main)
