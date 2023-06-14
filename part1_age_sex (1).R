# Read de CSV
df <- read.csv('Mortalidade_Geral_2021.csv', sep = ";")
head(df)
# Selecting the columns that I will use
# Rewritting in the same base to use less memory
df <- df[, c("IDADE", "SEXO", "RACACOR", "ESTCIV", "LOCOCOR", "NECROPSIA", "CIRCOBITO")]
head(df)

# Countting the NA  
sum(is.na(df$ESTCIV))
sum(is.na(df$CIRCOBITO))
sum(is.na(df$RACACOR))
sum(is.na(df$NECROPSIA))


### summing and plotting some variables ###
# Countting some information

# first, age
df$IDADE
# Less than 1 year <1
soma1 <- sum(df$IDADE < 400)
soma1
# Less than 18 1-18
soma2 <- sum(df$IDADE > 400 & df$IDADE < 418)
soma2
# Between 19-40
soma3 <- sum(df$IDADE > 418 & df$IDADE < 441)
soma3
# Between 41-60
soma4 <- sum(df$IDADE > 440 & df$IDADE < 461)
soma4
# 61 - 80
soma5 <- sum(df$IDADE > 460 & df$IDADE < 481)
soma5
# 81 - 99
soma6 <- sum(df$IDADE > 480 & df$IDADE < 500)
soma6
# More than 100 years 100>
soma7 <- sum(df$IDADE > 499)
soma7
# Putting de sums together
valores <- c(soma1, soma2, soma3, soma4, soma5, soma6, soma7)

# Setting the labels for the plot
rotulos <- c("< 1", "1 - 18", "19 - 40", "41 - 60", "61-80", " 81 - 99", "100 >")

# Settting the values by thousands
valores_new <- valores/1000

# Setting the colors 
colors <- c("#EDE9E3", "#EDE9E3", "#E7D7C9", "#CDC6C3", "#D4B2A7", "#CDC6C3", "#EDE9E3")
# Creatting the bar plot
barplot(as.numeric(valores_new), names.arg = rotulos, xlab = "Idade", ylab = "Qnt em milhares", main = "Mortalidade por Idade", col = colors)


### gender ####
df$SEXO
masc <- sum(df$SEXO == 1)
fem <- sum(df$SEXO == 2)
sum(fem)
sum(masc)
# In thousands 
sex <- c(masc, fem)
total_valueSex <- masc+fem
percent_sex <- c((masc/total_valueSex)*100, (fem/total_valueSex)*100)
format_sex <- sprintf("%.2f",percent_sex)

label_gender <- c("Masculino", "Feminino")

colors_gender <- c("#CDC6C3", "#FA8072") 

pie(sex, labels = format_sex, col = colors_gender, main = "Mortalidade por Sexo dos indivíduos")
legend("topright", legend = label_gender, fill = colors_gender, bty = "n")
# estado civil #
df$ESTCIV
solteiro <- sum((df$ESTCIV == 1)/1000, na.rm = TRUE)
casado <- sum((df$ESTCIV == 2)/1000, na.rm = TRUE)
viuvo <- sum((df$ESTCIV == 3)/1000, na.rm = TRUE)
separado <- sum((df$ESTCIV == 4)/1000, na.rm = TRUE)

values_estCiv <- c(separado, viuvo, solteiro, casado)

label_estCiv <- c("Divorciado", "Viuvo", "Solteiro", "Casado")

colors_estCiv <- c("#EED7A1", "#F2CEC2", "#D7878A", "#B6C4A0")

barplot(as.numeric(values_estCiv), names.arg = label_estCiv, xlab = "Estado Civil", ylab = "Qnt em milhares", main = "Estádos Civis mais frequentes", col = colors_estCiv)

# race  color
branco <- sum((df$RACACOR == 1), na.rm = TRUE)
preta <- sum((df$ERACACOR == 2), na.rm = TRUE)
amarela <-sum((df$RACACOR == 3), na.rm = TRUE)
parda <- sum((df$RACACOR == 4), na.rm = TRUE)

# Necropsia #
# Refere-se a execução ou não de necropsia para confirmação do diagnóstico.
# (1 – sim; 2 – não; 9 – ignorado)

df$NECROPSIA
necropsiaS1 <- sum(df$NECROPSIA == 1, na.rm = TRUE)
necropsias2 <- sum(df$NECROPSIA == 2, na.rm = TRUE)
sum(df$NECROPSIA == 9, na.rm = TRUE)

values_nec <- c(necropsiaS1/1000, necropsias2/1000)
valor_total <- (necropsiaS1 + necropsias2)

percent_nec <- c((necropsiaS1/valor_total)*100, (necropsias2/valor_total)*100)
format_percent <- sprintf("%.2f", percent_nec)

label_nec <- c("Sim", "Não")

color_nec <- c("#CDC6C3", "#FA8072")

pie(values_nec, labels = format_percent, col = color_nec, main = "Houve necrópsia para confirmar o diagnostico? ")
legend("topright", legend = label_nec, fill = color_nec, bty = "n")

# Tipo de morte violenta #
# Tipo de morte violenta ou circunstâncias em que se deu a morte não natural.
# (1 – acidente; 2 – suicídio; 3 – homicídio; 4 – outros; 9 – ignorado)

df$CIRCOBITO
sum(is.na(df$CIRCOBITO))
# 200.000 
cib_acidente <- sum(df$CIRCOBITO == 1, na.rm = TRUE)
cib_suic <- sum(df$CIRCOBITO == 2, na.rm = TRUE)
cid_homi <- sum(df$CIRCOBITO == 3, na.rm = TRUE)
cid_outros <- sum(df$CIRCOBITO == 4, na.rm = TRUE)

values_cid <- c(cib_acidente, cib_suic, cid_homi, cid_outros)

label_cib <- c("Acidente", "Suícidio", "Homicídio", "Outros")

color_cib <- c("", "", "", "")

barplot(as.numeric(values_cid), names.arg = label_cib, xlab = "Tipo de morte", ylab = "Quantidade", main = "Mortes violentas e não naturais")


#  Local do óbito #
df$LOCOCOR
sum(is.na(df$LOCOCOR))

loc_hospital <- sum(df$LOCOCOR == 1, na.rm = TRUE)
loc_saude <- sum(df$LOCOCOR == 2, na.rm = TRUE)
loc_domicilio <- sum(df$LOCOCOR == 3, na.rm = TRUE)
loc_viaPub <- sum(df$LOCOCOR == 4, na.rm = TRUE)
loc_outros <- sum(df$LOCOCOR == 5, na.rm = TRUE)

values_loc <- c(loc_hospital/1000, loc_saude/1000, loc_domicilio/1000, loc_viaPub/1000, loc_outros/1000)

label_loc <- c("Hospital", "Estabelecimento de saúde", "Domicílio", "Via pública", "Outros")

color_loc <- c("", "", "", "", "")

barplot(as.numeric(values_loc), names.arg = label_loc, xlab = "Local", ylab = "Quantidade em 1000", main = "Locais dos óbitos")
sum_locO <- (loc_saude + loc_viaPub + loc_outros)
sum_locO



