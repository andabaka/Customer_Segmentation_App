k_mean_fn <-
function(data, k_means, seed = NULL){
    if(identical(k_means, "Age")) {
        k_means_tbl <- data %>% 
            select("Spending_Score", "Age") %>% 
            scale() %>% 
            kmeans(centers = 4, nstart = 100)
    }
    else if(identical(k_means, "Annual_Income")) {
        k_means_tbl <- data %>% 
            select("Spending_Score","Annual_Income") %>% 
            scale() %>% 
            kmeans(centers = 5, nstart = 100)
    }
    
    else if(identical(k_means, c("Age", "Annual_Income"))) {
        k_means_tbl <- data %>% 
            select("Spending_Score","Age", "Annual_Income") %>% 
            scale() %>% 
            kmeans(centers = 6, nstart = 100)
    }
    
    return(k_means_tbl)
}
umap_fn <-
function(data, k_means) {
    custom.config <- umap.defaults
    custom.config$random_state <- 123
    if(identical(k_means, "Age")) {
        umap_obj <- data %>% 
            select("Spending_Score", "Age") %>% 
            scale() %>% 
            umap(config = custom.config)
    }
    else if(identical(k_means, "Annual_Income")){
        umap_obj <- data %>% 
            select("Spending_Score","Annual_Income") %>% 
            scale() %>% 
            umap(config = custom.config)
    }
    else if(identical(k_means, c("Age", "Annual_Income"))){
        umap_obj <- data %>% 
            select("Spending_Score","Annual_Income", "Age") %>% 
            scale() %>% 
            umap(config = custom.config)
    }
    
    umap_results_tbl <- umap_obj$layout %>% 
        as_tibble() %>% 
        set_names("x", "y") %>% 
        bind_cols(data %>% select(CustomerID))
    
    return(umap_results_tbl)
    
}

