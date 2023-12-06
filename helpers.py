import subprocess
import os
import re

# Class for expressing queries: consists of query type, subject, predicate, and object
class Query:
    def __init__(self, type, subject, predicate, object):
        self.type = type
        self.subject = subject
        self.predicate = predicate
        self.object = object

    # Default assembly of query (as seen in regulates-EGFR, diabetes-causes, and diabetes-treatments)
    def assemble_query(self):
        if "scored" in self.type:
            raise ValueError("Application of scored query should use assemble_fast_query rather than assemble_query \nQuery info: \n type: " + self.type + "\n subject: " + self.subject + "\n predicate: " + self.predicate + "\n object: " + self.object)
        if self.type == "X->Known":
            return '''(time (query:''' + self.type + '''
            ''' + self.subject + '''
            (set->list
            (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
            '("''' + self.predicate + '''"))) 
            (set->list
            (get-descendent-curies*-in-db
            (curies->synonyms-in-db (list "''' + self.object + '''"))))))'''
        elif self.type == "Known->X":
            return '''(time (query:''' + self.type + '''
            (set->list
            (get-descendent-curies*-in-db
            (curies->synonyms-in-db (list "''' + self.subject + '''"))))
            (set->list
            (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
            '("''' + self.predicate + '''")))
            ''' + self.object + ''')))'''

    # Assembly of query that uses buckets to be faster (as seen in regulates-EGFR-faster and imatinib-regulates-faster)
    def assemble_fast_query(self):
        if "scored" not in self.type:
            raise ValueError("Application of unscored-type query should not use assemble_fast_query \nQuery info: \n type: " + self.type + "\n subject: " + self.subject + "\n predicate: " + self.predicate + "\n object: " + self.object)
        if self.type == "X->Known-scored":
            return '''(time (query:''' + self.type + '''
            ''' + self.subject + '''
            (set->list
            (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
            '("''' + self.predicate + '''")))
            (set->list
            (get-descendent-curies*-in-db
            (curies->synonyms-in-db (list "''' + self.subject + '''"))))
            TOP_BUCKET_NUMBERS
            ))'''
        elif self.type == "Known->X-scored":
            return '''(time (query:''' + self.type + '''
            (set->list
            (get-descendent-curies*-in-db
            (curies->synonyms-in-db (list " '''+ self.subject + '''"))))
            (set->list
            (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
            '("''' + self.predicate + '''")))
            ''' + self.object + '''
            TOP_BUCKET_NUMBERS))'''

    # Assembly of minimal-syntax query
    def assemble_base_query(self):
        if "scored" in self.type:
            raise ValueError("Application of scored query should use assemble_fast_query rather than assemble_base_query \nQuery info: \n type: " + self.type + "\n subject: " + self.subject + "\n predicate: " + self.predicate + "\n object: " + self.object)
        if self.type == "X->Known":
            return '''(query:''' + self.type + '''
            ''' + self.subject + '''
            (set->list
            (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
            '("''' + self.predicate + ''' "))) 
            (set->list
            (get-descendent-curies*-in-db
            (list "''' + self.object + '''"))))'''

# Translate query output string to table by checking for "a" "b" "c" format
def table_output(data):
    pattern = r'\"([^\"]+)\"\s+\"([^\"]+)\"\s+\"([^\"]+)\"'
    matches = re.findall(pattern, data)
    result = [list(match) for match in matches]
    return result

# Writes the query (expressed as string) to the specified file path
def write(file_path, write_string):
    write_string = '''
    #lang racket/base

    (require
    "../../../../medikanren2/neo/neo-low-level/query-low-level-multi-db.rkt"
    "../../../../medikanren2/neo/neo-utils/neo-helpers-multi-db.rkt"
    json
    racket/format
    racket/list
    racket/match
    racket/set
    racket/pretty
    racket/string)

    (define robokop-top-bucket (list 5)) ;top/max bucket num of RoboKop KG
    (define text-mining-top-bucket (list 5)) ;top/max bucket num of Text Mining KG
    (define rtx-kg2-top-bucket (list 7)) ;top/max bucket num of RTX-KG2 KG

    ; Numbers of the top buckets of RoboKop KG, Text Mining KG, and RTX-KG2 KG (in this order).
    ; [The higer the bucket number, the higher amount of publications supporting the edge]
    (define TOP_BUCKET_NUMBERS (list robokop-top-bucket
                                    text-mining-top-bucket
                                    rtx-kg2-top-bucket
                                    ))\n''' + write_string
    with open(file_path, "w+") as file:
        file.write(write_string)

# Runs the racket script in the designated file path
def run(file_path):
    command = f"racket {file_path}"
    print(command)
    process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()

    if process.returncode != 0:
        print("Error:", stderr.decode())
        return stderr.decode()
    else:
        print("Output:", stdout.decode())
        return stdout.decode()


# Combination of the write and run functions: writes the query string to the file, and then runs it
def write_and_run(file_path, write_string):
    write_string = '''
    #lang racket/base

    (require
    "../../../../medikanren2/neo/neo-low-level/query-low-level-multi-db.rkt"
    "../../../../medikanren2/neo/neo-utils/neo-helpers-multi-db.rkt"
    json
    racket/format
    racket/list
    racket/match
    racket/set
    racket/pretty
    racket/string)

    (define robokop-top-bucket (list 5)) ;top/max bucket num of RoboKop KG
    (define text-mining-top-bucket (list 5)) ;top/max bucket num of Text Mining KG
    (define rtx-kg2-top-bucket (list 7)) ;top/max bucket num of RTX-KG2 KG

    ; Numbers of the top buckets of RoboKop KG, Text Mining KG, and RTX-KG2 KG (in this order).
    ; [The higer the bucket number, the higher amount of publications supporting the edge]
    (define TOP_BUCKET_NUMBERS (list robokop-top-bucket 
                                    text-mining-top-bucket 
                                    rtx-kg2-top-bucket 
                                    ))\n''' + write_string

    with open(file_path, "w+") as file:
         file.write(write_string)
    command = f"racket {file_path}"
    process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()

    if process.returncode != 0:
        print("Error:", stderr.decode())
        return stderr.decode()
    else:
        print("Output:", stdout.decode())
        return stdout.decode()

# Define a 'hop' between the table results of two queries
def hop(list1, list2):
    list1.sort(key=lambda x: x[-1])
    list2.sort(key=lambda x: x[0])
    new_list = []
    i, j = 0, 0
    while i < len(list1) and j < len(list2):
        if list1[i][-1] == list2[j][0]:
            new_list.append([list1[i], list2[j]])
            i += 1
            j += 1
        elif list1[i][-1] < list2[j][0]:
            i += 1
        else:
            j += 1
    return new_list

# Define a 'hop' between two members of Query class
def query_hop(query1, query2, file_path):
    query_string1 = query1.assemble_query()
    query_string2 = query2.assemble_query()

    output1 = write_and_run(file_path, query_string1)
    parsed_output1 = table_output(output1)

    output2 = write_and_run(file_path, query_string2)
    parsed_output2 = table_output(output2)

    hop_result = hop(parsed_output1, parsed_output2)

    return hop_result



