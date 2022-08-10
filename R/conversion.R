#' Title
#'
#' @param gene_list character vector of mouse symbols
#'
#' @return character vector of converted human symbols
#' @export
#'
#' @examples
#' 
convert_mouse_to_human <- function(gene_list){
    require(data.table)
    mouse_human_genes = fread("http://www.informatics.jax.org/downloads/reports/HOM_MouseHumanSequence.rpt", data.table = F, check.names = T)
    
    output = c()
    
    for(gene in gene_list){
        class_key = (mouse_human_genes %>% filter(Symbol == gene & Common.Organism.Name=="mouse, laboratory"))[['DB.Class.Key']]
        if(!identical(class_key, integer(0)) ){
            human_genes = (mouse_human_genes %>% filter(DB.Class.Key == class_key & Common.Organism.Name=="human"))[,"Symbol"]
            for(human_gene in human_genes){
                output = append(output,human_gene)
            }
        }
    }
    
    return (output)
}