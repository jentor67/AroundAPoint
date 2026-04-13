#!/usr/bin/python3
import csv


# Load two datasets
file1 = "/home/jmajor/Git/AroundAPoint/data/file1.csv"
file2 = "/home/jmajor/Git/AroundAPoint/data/file2.csv"
file1 = "/mnt/kdrive/file_00000002.dat"
file2 = "/mnt/kdrive/file_00000003.dat"

def load_data(filepath):
    data = []
    with open(filepath, newline='') as csvfile:
        reader = csv.DictReader(csvfile, delimiter='|')
        #reader = csv.DictReader(csvfile)
        rown = 0
        #"frame": int(row["frame"]),
        #"frame": int(rown),
        for row in reader:
            data.append({
                "frame": int(row["frame"]),
                "x": float(row["x"]),
                "y": float(row["y"]),
                "z": float(row["z"]),
            })
            rown = rown + 1
    return data

data1 = load_data(file1)
data2 = load_data(file2)




