from helpers import *
ALL = "#f"

# Initialize scored-bucket query
sample_query2 = Query(type= "Known->X-scored", subject="PUBCHEM.COMPOUND:5291", predicate="biolink:regulates", object=ALL)

# Assemble bucket-scored query
fast_query = sample_query2.assemble_fast_query()

file_path = '/home/matt/mediKanren/contrib/medikanren2/neo/Matt_K/generated_query.rkt'

# Print resulting table
result_table = table_output(write_and_run(file_path, fast_query))
print(result_table)


