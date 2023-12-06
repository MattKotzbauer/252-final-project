from helpers import *
ALL = "#f"

# Initialize member of query class
sample_query1 = Query(type="X->Known", subject=ALL, predicate="biolink:regulates", object="HGNC:3236")

# Assemble query string based on class member
sample_default_query = sample_query1.assemble_query()

# Print direct terminal output of query
print(sample_default_query)

# Write query to file and run file
sample_path = '/home/matt/mediKanren/contrib/medikanren2/neo/Matt_K/generated_query.rkt'
query_output = write_and_run(sample_path, sample_default_query)

# Create formatted table from terminal output
table_output = table_output(query_output)

# (Print formatted table)
print(table_output)

# Create second sample query and use to demonstrate multi-hop queries
sample_query2 = Query(type="Known->X", subject="PUBCHEM.COMPOUND:5291", predicate="biolink:regulates", object=ALL)
sample_hop = query_hop(sample_query2, sample_query1, sample_path)




