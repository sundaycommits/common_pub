library(encryptr)
library(dplyr)

# generate keys : 
#genkeys()

 
# Without using a lookUp table ----------------
# ....
# encrypt example
data(gp)

gp_encrypt = gp %>% 
  select(-c(name, address1, address2, address3)) %>% 
  encrypt(postcode, telephone)

# decrypt 
gp_decrypt <- gp_encrypt %>% 
  decrypt(postcode, telephone)

# .... 


# Using a lookup table -----------------------
{
gp_encrypt = gp %>% 
  select(-c(name, address1, address2, address3)) %>% 
  encrypt(postcode, telephone, lookup = TRUE)



gp_decrypt <- gp_encrypt %>%  
  decrypt(postcode, telephone, lookup_object = lookup)

# OR : 

gp_decrypt <- gp_encrypt %>%  
  decrypt(postcode, telephone, lookup_path = "lookup.csv")
}

# Now encrypting a file altogether : ---------

write.csv(gp, "gp.csv")
encrypt_file("gp.csv")


decrypt_file("gp.csv.encryptr.bin", file_name = "gp3.csv")



# now Providing a public key : ---------------
gp_encrypt = gp %>% 
  select(-c(name, address1, address2, address3)) %>% 
  encrypt(postcode, telephone, public_key_path = "id_rsa.pub")









