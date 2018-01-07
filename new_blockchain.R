############################################
### Building a more sophisticated blockchain
############################################

# Create a blockchain

blockchain <- list(chain = list(),
                   currentTransactions = list())

## Add a new block. To add a new block we need the proof from the block before and the previous hash
  
create_new_block <- function(bc, proof, previousHash=NULL){
    previousHash <- ifelse (is.null(previousHash), 
                            hash_block(bc$chain[length(bc$chain)]),
                            previousHash)
    
    block = list('block' = list('index' = length(bc$chain) + 1,
                                'timestamp' = as.numeric(Sys.time()),
                                'transactions' =  bc$currentTransactions,
                                'proof' = proof,
                                'previousHash' = previousHash))
    
    bc$currentTransactions = NULL
    bc$chain <- append(bc$chain, block)
    return (block)
  }
      
### Add helper function which hashes a block
      
hash_block = function (block){
  digest(block, algo="sha256")
  }
  
## Add new transactions which takes three values: Sender, Recipient, and the amount
  
add_transaction = function(sender, recipient, amount){
    transaction_list <-  list('transaction'= list('sender'=sender,
                                                  'recipient'=recipient,
                                                  'amount'=amount))
    bc$currentTransactions <- append(bc$currentTransactions, transaction_list)
    last.block <- bc$chain[length(bc$chain)]
    return(last.block$block$index + 1)
  }
  
  
## Add a proof-of-work function:
proofOfWork <- function(last_proof){
  proof <- 0
  
  while (!bc$validProof(last_proof, proof)){
      proof <- proof + 1
  }
  
  return (proof)
  }
  
## Add function to see whetehr a proof is valid
  
validProof <- function (last_proof, proof){
  guess = paste0(last_proof,proof)
  guess_hash = digest(guess, algo = 'sha256')
  return (gsub('.*(.{2}$)', '\\1',guess_hash) == "00")
}

validChain <- function(chain)
{lastBlock <- chain[0]
  currentIndex <- 1
  while (currentIndex < length(chain)){
    block = chain[currentIndex]
    # checking for valid linking
    if (block$block$previousHash != bc$hashBlock(lastBlock)){
      return(FALSE)
    }
    # checking for proof validity
    if(!bc$validProof(lastBlock$block$proof, block$block$proof)){
      return (FALSE)
    }
    
    lastBlock <- block
    currentIndex <- currentIndex +1
  }
  return(TRUE)
}
  


### Simple Proof of Work Alogrithm

proof_of_work <- function(last_proof){
  proof <- last_proof + 1
  
  while (!(incrementor %% 9 == 0 & incrementor %% last_proof == 0 )){
    proof <- proof + 1
  }
  
  return(proof)
}
