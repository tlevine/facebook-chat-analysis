#!/usr/bin/env python
import pandas
import pandas.io.sql
import sqlite3

# IO DataFrame
def load():
    connection = sqlite3.connect('/tmp/logs.db')
    return pandas.io.sql.read_frame('SELECT * FROM "log_status";', connection)

# IO ()
def main():
    df = load()

