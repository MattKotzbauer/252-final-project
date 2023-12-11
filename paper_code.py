from helpers.py import *
ALL = "#f"

# Code from sections 1, 3, 4, and 5 of the report (probably should only run individual sections rather than whole file)

# Section 1: Introduction

# Initialize member of query class
sample_query = Query(type="X->Known", subject=ALL, predicate="biolink:regulates", object="HGNC:3236")

# Assemble query string based on class member
sample_default_query = sample_query.assemble_query()

# Write query to file and run it
query_output = write_and_run(~/mediKanren/query.rkt, sample_default_query)


# Section 3: DSL Step-Through

sample_query = Query(type="Known->X", subject="PUBCHEM.COMPOUND:5291", predicate="biolink:regulates", object=ALL)
query_string = assemble_query(sample_query)

formulated_output = table_output(query_output)

fast_query = Query(type="Known->X", subject="PUBCHEM.COMPOUND:5291", predicate="biolink:regulates", object=ALL)
fast_query_string = assemble_fast_query(fast_query)

query_1 = Query(type="X->Known", subject=ALL, predicate="biolink:regulates", object="HGNC:3236")
query_2 = Query(type="Known->X", subject="PUBCHEM.COMPOUND:5291", predicate="biolink:regulates", object=ALL)
sample_hop = query_hop(query_1, query_2)

table_1 = table_output(write_and_run(query_1))
table_2 = table_output(write_and_run(query_2))
sample_hop = table_hop(table_1, table_2)

table_3 = table_output(write_and_run(Query(type = "Known->X", subject=ALL, predicate="biolink:regulates", object="PUBCHEM.COMPOUND:2244")))
multi_hop = table_hop(table_3, sample_hop)


# Section 4: DSL Design
'''
# Default assembly of query (as seen in regulates-EGFR, diabetes-causes, and diabetes-treatments)
assemble_query(input_query: Query) -> str:

# Assembly of query that uses buckets to be faster (as seen in regulates-EGFR-faster and imatinib-regulates-faster)
assemble_fast_query(input_query: Query) -> str:

# Assembly of minimal-syntax query
assemble_base_query(input_query: Query) -> str:

# Writes string as Racket script to file path
write(file_path: str, write_string: str) -> void

# Runs Racket script specified by file path (returns the terminal output from the query)
run(file_path: str) -> str

# Combination of write and run functions: writes string as Racket script to file path and runs it (returns the terminal output from the query)
write_and_run(file_path: str, write_string: str) -> str

# Translate query output string to table by parsing in Python
table_output(terminal_output: str) -> List[List[Any]]
# (Resulting list form: [l1, ..., ln], where li = [subject, predicate, object, {key_1: aux_list_1, ..., key_m: aux_list_m}] )

# Defines a hop between two queries
query_hop(query1: Query, query2: Query, file_path: str) -> List[List[Any]]

# Defines a hop between the table results of two queries
table_hop(table1: List[List[Any]], table2: List[List[Any]]) -> List[List[Any]]
'''

# Section 5: DSL Demo

std_query = Query(type="X->Known", subject=ALL, predicate="biolink:regulates", object="HGNC:3236")
query_string = assemble_query(std_query)
result_string = write_and_run("~/sample.rkt", std_query)
result_table = table_output(result_string)

sample_query1 = Query(type="Known->X", subject="PUBCHEM.COMPOUND:441207", predicate="biolink:regulates", object=ALL)
sample_query2 = Query(type="X->Known", subject=ALL, predicate="biolink:regulates", object="HGNC:3236")
hop_result = query_hop(sample_query1, sample_query2)






