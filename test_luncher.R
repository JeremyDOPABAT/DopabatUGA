
#res_arxiv=extraction_data_api_arxiv(data_pub=ths,ti_name=ti_name,au_name=au_name,pas=8,value_same_min_accept=0.95,value_same_min_ask = 0.85,type = "all")
#48 mi avec la version 8 auteur 147 resultat 
#dim(test_luncher_results$res_arxiv$res_publi_foundt)
res_data_nasa_ads=extraction_data_api_nasa_ads(data_pub=ths,ti_name=ti_name,au_name=au_name,token=token,pas=8,value_same_min_accept=0.95,value_same_min_ask = 0,type="all",sep_vector_in_data ="sep",position_vector_in_data = "position_name" )