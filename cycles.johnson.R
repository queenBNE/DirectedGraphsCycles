#######################################################################################################
# This is an implementation of Johnson's algorithm for finding all the elementary circuits of a       #
# directed graph.                                                                                     #
# An elementary circuit is a circuit where no vertex but the first and last vertex appears twice.     #
# Two elementary circuits are distinct if one is not a cyclic permutation of the other.               #
# The details on this algorithm may be found in;                                                      #
#                                                                                                     #
# Finding all the elementary circuits of a directed graph - Donald B. Johnson, 1974.                  #
#                                                                                                     #  
# Author: Corrie Jacobien Carstens                                                                    #
#######################################################################################################

library(igraph)

# Function to obtain the strong component K with least vertex in subgraph of g induced by {s,s+1, ..., n}
# So the strong component of the subgraph on vertex s,s+1,...,n that contains the least vertex. 
# Note that this function may return NULL if no such component exists. 
get.induced.strong <- function(g, s){
  # Create the induced subgraph on {s, s+1, ..., n} and compute the strong components
  sg <- induced.subgraph(g, vids=s:vcount(g))
  sc <- clusters(sg, mode="strong")
  
  # Obtain the names for the remaining nodes - this has to be done to make sure that we use
  # the right order of nodes, we want to find the strong component with the least vertex.
  # Igraph always uses ids 1:n' for a graph, so we need to use the names. 
  ids <- as.numeric(get.vertex.attribute(sg, "name", 1:vcount(sg)))
  order <- sort(ids, index.return=T)$ix
  
  # Obtain the vertices of the strong component with the least vertex
  others <- c() 
  for(v in order)
    if(length(others) <= 1)
      others <- which(sc$membership == sc$membership[v])
  
  # If there is a strong component with more than 1 vertex, return this component
  if(length(others) > 1)
    return(induced.subgraph(sg, others))
  # Else return NULL
  else 
    return(NULL)
}

# This function returns a list where u is unblocked and all vertices in B(u)
# are unblocked (recursively)
unblock <- function(u, b, B){
  b[u] <- F
  for(w in B[[u]]){
    B[[u]] <- B[[u]][-which(B[[u]]==w)]
    if(b[w]){
      bB <- unblock(w,b,B)
      b <- bB$b
      B <- bB$B
    }
  }
  return(list(b=b, B=B))
}

# Prints out the circuits starting at s 
circuit <- function(s, v, Ak, B, b, f, stack, ids){
  stack <- c(stack, v)
  b[v] <- T
  for(w in neighbors(Ak, v, mode="out")){
    if(w==s){
      print(sapply(c(stack,s), FUN=function(i){return(ids[i])}))
      f = T
    }else if (!b[w]){
      updated <- circuit(s,w,Ak,B,b,f,stack, ids)
      B <- updated$B
      b <- updated$b
      stack <- updated$stack      
      if(updated$f)
        f = T
    }
  }
  if(f){
    updated <- unblock(v, b, B)
    b <- updated$b
    B <- updated$B
  }else{for(w in neighbors(Ak, v, mode="out"))
    if (! v %in% B[[w]])
      B[[w]] <- c(B[[w]], v)
  }
  stack <- stack[-length(stack)]
  return(list(B=B, b=b, f=f, stack=stack))
}

# Main loop (L3)
get.elementary.circuits <- function(g){
  b <- rep(F, vcount(g))
  B <- vector("list", vcount(g))
  s = 1
  while(s < vcount(g)){
    Ak <- get.induced.strong(g,s)
    if(!is.null(Ak)){
      ids <- as.numeric(get.vertex.attribute(Ak, "name", 1:vcount(Ak)))
      s <- min(ids)
      for(i in ids){
        b[i] <- F
        B[[i]] <- numeric(0)
      }
      s_indx <- which(ids == s)
      circuit(s_indx, s_indx, Ak, B, b, F, numeric(0), ids)
      s <- s + 1
    }else
      s <- vcount(g) 
  }
}