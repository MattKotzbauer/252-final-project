from helpers import Query, write, run, write_and_run

sample_query = Query(type="X->Known", subject="ALL", predicate="biolink:regulates", object="HGNC:3236")
sample_default_query = sample_query.assemble_query()
print(sample_default_query)



