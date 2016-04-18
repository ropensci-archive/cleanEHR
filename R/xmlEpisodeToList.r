#' Note: it rules out any time data with missing values.
#' @export xmlEpisodeToList
xmlEpisodeToList <- function(episode_node) {
    traverseNode <- function(node) {
        if (is.null(node)) {
            #leaf node reached. Turn back
            return()
        }

        num.children = xmlSize(node)  
        if(num.children == 0 ) {
            label <- xmlName(xmlParent(node))
            if (is.null(.which.type(label))) {
                warning("XML structure error:", label)
            }
            else {
                # here we check if the id is 1d instead of checking the
                # parent node size, because the parent node size can be 1 due
                # to missing data of 2d items, which should not be recorded as
                # valide data, as a result missing data will be ignored. 
                if (.which.type(label) == "item1d") { 
                    node_env$data[[label]][[label]] <- xmlValue(node)
                }

                else {
#                    node_ <- xmlParent(node)
#                    parent_size <- xmlSize(xmlParent(node_))
#
#                    for (i in seq(parent_size)) {
#                        if (xmlName(node_) == xmlName(xmlParent(node_)[[1]]))
#                        {
#                            node_name <- xmlName(node_)
#                            if (i > 1) 
#                                node_ <- getSibling(node_)
#
#
#                            if (is.null(node_env$data[[label]][[node_name]]))
#                                node_env$data[[label]][[node_name]] <- vector()
#
#                            node_env$data[[label]][[node_name]] <-
#                                c(node_env$data[[label]][[node_name]],
#                                  xmlValue(node_[[1]]))
#                        }
#                    }
#                }
#            }
#        }
#






                    parent_size <- xmlSize(xmlParent(xmlParent(node)))
                            sib <- getSibling(xmlParent(node))
        
                            if (parent_size == 2) {
                                if (!is.null(sib)) {
                                    old_data <- node_env$data[[label]][[label]]
        
                                    if(is.null(old_data))
                                        node_env$data[[label]][[label]] <- vector()
                                    node_env$data[[label]][[label]] <- c(old_data, xmlValue(node))
        
                                    old_sibling <- node_env$data[[label]][[xmlName(sib)]]
                                    if (is.null(old_sibling))
                                        node_env$data[[label]][[xmlName(sib)]] <- vector()
                                    node_env$data[[label]][[xmlName(sib)]] <-
                                        c(old_sibling, xmlValue(sib))
                                }
                            }
                            else if (parent_size == 3) {
                                if (!is.null(sib)){
                                    sibsib <- getSibling(sib)
                                    if (!is.null(sibsib)) {
                                        sibling1_name <- xmlName(sib)
                                        sibling2_name <- xmlName(sibsib)
        
                                        old_data <- node_env$data[[label]][[label]]
                                        old_sibling1 <- node_env$data[[label]][[sibling1_name]]
                                        old_sibling2 <- node_env$data[[label]][[sibling2_name]]
        
                                        if (is.null(old_data))
                                            node_env$data[[label]][[label]] <- vector()
                                        if (is.null(old_sibling1))
                                            node_env$data[[label]][[sibling1_name]] <- vector()
                                        if (is.null(old_sibling2))
                                            node_env$data[[label]][[sibling2_name]] <- vector()
        
                                        node_env$data[[label]][[label]] <- xmlValue(node)
                                        node_env$data[[label]][[sibling1_name]] <-
                                            xmlValue(sib)
                                        node_env$data[[label]][[sibling2_name]] <-
                                            xmlValue(sibsib)
        
                                    }
                                }
                            }
        
                        }
                    }
                }

        #Go one level denode_env$dataer
        for (i in 1 : num.children) {
            if (xmlValue(node) == "") {
                warning("XML structure is wrong ", xmlName(node))
                next
            }
            traverseNode(node[[i]]) #the i-th child of node
        }
    }

    node_env <- new.env()
    node_env$data <- list() # store data from the XML traverser 
    traverseNode(episode_node)
    node_env$ccdata <- list() # format can be directely called by ccEpisode.

    # rearrange the vector data from node_env$data to ccdata format.
    # Regarding to the performance, XML traverser only put all the data in a
    # vector form, as vector appending is much faster than data.frame
    # appending.
    lapply(node_env$data, function(x) {
               len <- length(x)
               if (len == 1) { # 1d item (simple item)
                   if (.which.type(names(x)) == "item1d" & is.character(x[[1]]))
                       node_env$ccdata[[names(x)]] <- as.character(x)
                   else
                       print(node_env$ccdata[[names(x)]])
                       stop("wrong simple data ", names(x))
               }
               else if (len == 2) { # 2d item (items in time)
                   nm <- c(.which.type(names(x)[1]), .which.type(names(x)[2]))
                   label <- names(x)[nm == "item2d"]
                   node_env$ccdata[[label]] <- data.frame(x[[1]], x[[2]])
                   names(node_env$ccdata[[label]]) <- nm
               }
               else if (len == 3) { # time data with meta data, i.e. 3 columns
                   nm <- c(.which.type(names(x)[1]), .which.type(names(x)[2]),
                           .which.type(names(x)[3]))
                   label <- names(x)[nm == "item2d"]
                   print(paste("=====", label))
                   print(names(x))
                   print(nm)
                   print(node_env$ccdata[[label]])
                   #print(data.frame(x[[1]], x[[2]], x[[3]]))
                   node_env$ccdata[[label]] <- data.frame(x[[1]], x[[2]], x[[3]])
                   names(node_env$ccdata[[label]]) <- nm
                                  }})

    return(node_env$ccdata)
}
