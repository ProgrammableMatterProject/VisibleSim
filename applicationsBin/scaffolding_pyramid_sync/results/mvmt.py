import csv

with open('mvmt.log') as csv_file:
    csv_reader = csv.reader(csv_file, delimiter='\t')
    data=dict()
    for row in csv_reader:
        if row[0] in data:
            data[row[0]] += 1
        else:
            data[row[0]] = 1
        # print(f'data[{row[0]}] = {data[row[0]]}')

    for key in data:
        print(f'{key}\t{data[key]}')
