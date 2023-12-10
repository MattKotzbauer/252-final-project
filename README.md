



```python
# Default assembly of query (as seen in regulates-EGFR, diabetes-causes, and diabetes-treatments)
assemble_query(input_query: Query) -> str:

# Assembly of query that uses buckets to be faster (as seen in regulates-EGFR-faster and imatinib-regulates-faster)
assemble_fast_query(input_query: Query) -> str:

# Assembly of minimal-syntax query
assemble_base_query(input_query: Query) -> str:


# Writes string as Racket script to file path
write(file_path: str, write_string: str) -> void

# Runs Racket script specified by file path
run(file_path: str) -> str

# Combination of write and run functions: writes string as Racket script to file path and runs it
write_and_run(file_path: str, write_string: str) -> str

k

Translate query output string to table by parsing in Python
table_output(terminal_output: str) -> List[List[Any]]

Resulting list form: [l1, ..., ln], where li = [subject, predicate, object, {key_1: aux_list_1, ..., key_m: aux_list_m}]



Defines a hop between two queries
query_hop(query1: Query, query2: Query, file_path: str) -> List[List[Any]]

Defines a hop between the table results of two queries
table_hop(table1: List[List[Any]], table2: List[List[Any]]) -> List[List[Any]]




