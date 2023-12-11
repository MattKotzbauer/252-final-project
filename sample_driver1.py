from helpers import *
ALL = "#f"

# Initialize query (as member of Query class)
sample_query1 = Query(type="X->Known", subject=ALL, predicate="biolink:regulates", object="HGNC:3236")

# Assemble query string based on class member
sample_default_query = sample_query1.assemble_query()

# Print direct terminal output of query
print(sample_default_query)

# Write query to file and run file
sample_path = '/home/matt/mediKanren/contrib/medikanren2/neo/Matt_K/generated_query.rkt'
query_output = write_and_run(sample_path, sample_default_query)


# (Print formatted table)
print(table_output)


