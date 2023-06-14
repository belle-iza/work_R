# Read de CSV
df <- read.csv('Mortalidade_Geral_2021.csv', sep = ";")
head(df)
# Selecting the columns that I will use
# Rewritting in the same base to use less memory
df <- df[, c("IDADE", "SEXO", "RACACOR", "ESTCIV", "LOCOCOR", "NECROPSIA", "CIRCOBITO")]
head(df)

### distribuição de óbitos por idade e sexo ###

############ SEPARANDO AS IDADES ###############
# Age between 1-18
age1 <- (df$IDADE > 400 & df$IDADE < 418)
sum(age1)
# Age between 19-40
age2 <- (df$IDADE > 418 & df$IDADE < 441)
sum(age2)  
# Age between 41-60
age3 <- (df$IDADE > 440 & df$IDADE < 461)
sum(age3)  
# Age between 61-80
age4 <- (df$IDADE > 460 & df$IDADE < 481)
sum(age4)  
# Age between 81-99
age5 <- (df$IDADE > 480 & df$IDADE < 500)
sum(age5)  
# Age between >100
age6 <- (df$IDADE > 499)
sum(age6)

######################## SEPARANDO OS SEXOS ##################
# Fem sex
sex_fem <- (df$SEXO == 2)
sum(sex_fem)

# Masc sex
sex_masc <- (df$SEXO == 1)
sum(sex_masc)
########### SEXO FEMININO POR IDADE #################
# 1-18 & fem
age1_fem <- (age1 & sex_fem)
sum(age1_fem)

# 19-40 & fem
age2_fem <- (age2 & sex_fem)
sum(age2_fem)

# 41-60 & fem
age3_fem <- (age3 & sex_fem)
sum(age3_fem)

# 61-80 & fem
age4_fem <- (age4 & sex_fem)
sum(age4_fem)

#  81-99 & fem
age5_fem <- (age5 & sex_fem)
sum(age5_fem)

# 100 > & fem
age6_fem <- (age6 & sex_fem)
sum(age6_fem)

# Separando os valores 
age_fem <- c(sum(age1_fem), sum(age2_fem), sum(age3_fem), sum(age4_fem), sum(age5_fem), sum(age6_fem))
# labels
label_age <- c("1-18", "19-40", "41-60", "61-80", "81-99", "100 >")

barplot(age_fem/1000, names.arg = label_age, xlab = "Idade", ylab = "Quantidade em 1000", main = "Mortes do sexo feminino separado por idade")
# Plot com apenas algumas variáveis

age_22 <- c(sum(age2_fem), sum(age2_masc), sum(age3_fem), sum(age3_masc))

label_22 <- c("19-40 (F)", "19-40 (M)", "41-60 (F)", "41-60 (M)")

colors_22 <- c("#FA8072", "#CDC6C3", "#FA8072", "#CDC6C3") 

barplot(age_22/1000, names.arg = label_22, xlab = "Idade e sexo", ylab = "Quantidade em 1000", main = "Mortes feminino x masculino", col = colors_22)
################### SEXO MASCULINO POR IDADE #################
# 1-18 & masc
age1_masc <- (age1 & sex_masc)
sum(age1_masc)

# 19-40 & fem
age2_masc <- (age2 & sex_masc)
sum(age2_masc)

# 41-60 & fem
age3_masc <- (age3 & sex_masc)
sum(age3_masc)

# 61-80 & fem
age4_masc <- (age4 & sex_masc)
sum(age4_masc)

#  81-99 & fem
age5_masc <- (age5 & sex_masc)
sum(age5_masc)

# 100 > & fem
age6_masc <- (age6 & sex_masc)
sum(age6_masc)

# Separando os valores 
age_masc <- c(sum(age1_masc), sum(age2_masc), sum(age3_masc), sum(age4_masc), sum(age5_masc), sum(age6_masc))

barplot(age_masc/1000, names.arg = label_age, xlab = "Idade", ylab = "Quantidade em 1000", main = "Mortes do sexo masculino separado por idade")


### sexo e tipo de morte violenta ###

########## separando os tipos de mortes #############

tipo_acidente <- (df$CIRCOBITO == 1)
sum(tipo_acidente, na.rm = TRUE)

tipo_suicidio <- (df$CIRCOBITO == 2)
sum(tipo_suicidio, na.rm = TRUE)

tipo_homicidio <- (df$CIRCOBITO == 3)
sum(tipo_homicidio, na.rm = TRUE)

tipo_outros <- (df$CIRCOBITO == 4)
sum(tipo_outros, na.rm = TRUE)

label_obito <- c("Acidente", "Homicidio", "Suicidio")

####### CRUZANDO ############
########## FEM & TIPO DOS ÓBITOS ##########
tp_acid_fem <- (sex_fem & tipo_acidente)
sum(tp_acid_fem, na.rm = TRUE)

tp_suic_fem <- (sex_fem & tipo_suicidio)
sum(tp_suic_fem, na.rm = TRUE)

tp_homic_fem <- (sex_fem & tipo_homicidio)
sum(tp_homic_fem, na.rm = TRUE)

tp_outros_fem <- (sex_fem & tipo_outros)
sum(tp_outros_fem, na.rm = TRUE)

# values
tipo_obito_fem <- c(sum(tp_acid_fem, na.rm = TRUE),sum(tp_suic_fem, na.rm = TRUE) ,sum(tp_suic_fem, na.rm = TRUE))

barplot(tipo_obito_fem/1000,names.arg = label_obito, xlab = "Tipo de morte", ylab = "Quantidade em 1000", main = "Tipos de mortes não naturais do sexo feminino")

############# MASC & TIPO DOS ÓBITOS ###########

tp_acid_masc <- (sex_masc & tipo_acidente)
sum(tp_acid_masc, na.rm = TRUE)

tp_suic_masc <- (sex_masc & tipo_suicidio)
sum(tp_suic_masc, na.rm = TRUE)

tp_homic_masc <- (sex_masc & tipo_homicidio)
sum(tp_homic_masc, na.rm = TRUE)

tp_outros_masc <- (sex_masc & tipo_outros)
sum(tp_outros_masc, na.rm = TRUE)

tipo_obito_masc <- c(sum(tp_acid_masc, na.rm = TRUE),sum(tp_suic_masc, na.rm = TRUE) ,sum(tp_suic_masc, na.rm = TRUE))

barplot(tipo_obito_masc/1000,names.arg = label_obito, xlab = "Tipo de morte", ylab = "Quantidade em 1000", main = "Tipos de mortes não naturais do sexo masculino")

### necropsia e sexo ###
### NECROPSIA ###

n_sim <- (df$NECROPSIA == 1)
n_nao <- (df$NECROPSIA == 2)
label_necro <- c("Sim", "Não")

###### CRUZANDO ######
# FEMININO

necro_fem_s <- (n_sim & sex_fem)
sum(necro_fem_s, na.rm =  TRUE)
necro_fem_n <- (n_nao & sex_fem)
sum(necro_fem_n, na.rm =  TRUE)

n_fem_values <- c(sum(necro_fem_s, na.rm =  TRUE), sum(necro_fem_n, na.rm =  TRUE))
n_fem_label <- c("Sim", "Não")

total_fem <- c(sum(necro_fem_s, na.rm =  TRUE) + sum(necro_fem_n, na.rm =  TRUE))
porcent_fem <- c((32180/total_fem)*100, (507721/total_fem)*100)
## arrumar a porcentagem 
format_fem_neco <- sprintf("%.2f",porcent_fem)
format_fem_neco


pie(n_fem_values, labels = format_fem_neco, main = "Necropsia do sexo fem", col = colors_necro)
legend("topright", legend = label_necro, fill = colors_necro, bty = "n")



# MASCULINO 

necro_masc_s <- (n_sim & sex_masc)
sum(necro_masc_s, na.rm = TRUE)
necro_masc_n <- (n_nao & sex_masc)
sum(necro_masc_n, na.rm = TRUE)

n_masc_values <- c(sum(necro_masc_s, na.rm =  TRUE), sum(necro_masc_n, na.rm =  TRUE))
n_masc_label <- c("Sim", "Não")



# Pegando a porcentagem
total_masc_neco <- c(sum(necro_masc_s, na.rm = TRUE) + sum(necro_masc_n, na.rm = TRUE))
percent_masc_neco <- c((110150/total_masc_neco)*100 , (573598/total_masc_neco)*100)

#Arrumando pra ficar com 2 casas decimais 
format_masc_neco <- sprintf("%.2f",percent_masc_neco)
format_masc_neco

colors_necro <- c("#FA8072", "#D4B2A7")

pie(n_masc_values, labels = format_masc_neco, main = "Necropsia do sexo masc", col = colors_necro)
legend("topright", legend = label_necro, fill = colors_necro, bty = "n")


### idade e morte violenta ###

####### CRUZANDO #########
# 1-18
age_1_acid <- (tipo_acidente & age1)
sum(age_1_acid, na.rm = TRUE)

age_1_homi <- (tipo_homicidio & age1)
sum(age_1_homi, na.rm = TRUE)

age_1_outros <- (tipo_outros & age1)
sum(age_1_outros, na.rm = TRUE)

age_1_suic <- (tipo_suicidio & age1)
sum(age_1_suic, na.rm = TRUE)

age1_acid <- c(sum(age_1_acid, na.rm = TRUE), sum(age_1_homi, na.rm = TRUE), sum(age_1_suic, na.rm = TRUE))

barplot(age1_acid, names.arg = label_obito, xlab = "Tipo de morte", ylab = "Quantidade", main = "Mortes entre 1-18 anos")

# 19-40

age_2_acid <- (tipo_acidente & age2)
sum(age_2_acid, na.rm = TRUE)

age_2_homi <- (tipo_homicidio & age2)
sum(age_2_homi, na.rm = TRUE)

age_2_outros <- (tipo_outros & age2)
sum(age_2_outros, na.rm = TRUE)

age_2_suic <- (tipo_suicidio & age2)
sum(age_2_suic, na.rm = TRUE)

age2_acid <- c(sum(age_2_acid, na.rm = TRUE), sum(age_2_homi, na.rm = TRUE), sum(age_2_suic, na.rm = TRUE))

color_tipo <- c("#D4B2A7", "#FA8072", "#D4B2A7")
barplot(age2_acid, names.arg = label_obito, xlab = "Tipo de morte", ylab = "Quantidade", main = "Mortes entre 19-40 anos", col = color_tipo)
# Unico momento que o numero por homicios é o maior 

# 41-60

age_3_acid <- (tipo_acidente & age3)
sum(age_3_acid, na.rm = TRUE)

age_3_homi <- (tipo_homicidio & age3)
sum(age_3_homi, na.rm = TRUE)

age_3_outros <- (tipo_outros & age3)
sum(age_3_outros, na.rm = TRUE)

age_3_suic <- (tipo_suicidio & age3)
sum(age_3_suic, na.rm = TRUE)

age3_acid <- c(sum(age_3_acid, na.rm = TRUE), sum(age_3_homi, na.rm = TRUE), sum(age_3_suic, na.rm = TRUE))

barplot(age3_acid, names.arg = label_obito, xlab = "Tipo de morte", ylab = "Quantidade", main = "Mortes entre 41-60 anos")


# 61-80

age_4_acid <- (tipo_acidente & age4)
sum(age_4_acid, na.rm = TRUE)

age_4_homi <- (tipo_homicidio & age4)
sum(age_4_homi, na.rm = TRUE)

age_4_outros <- (tipo_outros & age4)
sum(age_4_outros, na.rm = TRUE)

age_4_suic <- (tipo_suicidio & age4)
sum(age_4_suic, na.rm = TRUE)

age4_acid <- c(sum(age_4_acid, na.rm = TRUE), sum(age_4_homi, na.rm = TRUE), sum(age_4_suic, na.rm = TRUE))

barplot(age4_acid, names.arg = label_obito, xlab = "Tipo de morte", ylab = "Quantidade", main = "Mortes entre 61-80 anos")


# 81-99

age_5_acid <- (tipo_acidente & age5)
sum(age_5_acid, na.rm = TRUE)

age_5_homi <- (tipo_homicidio & age5)
sum(age_5_homi, na.rm = TRUE)

age_5_outros <- (tipo_outros & age5)
sum(age_5_outros, na.rm = TRUE)

age_5_suic <- (tipo_suicidio & age5)
sum(age_5_suic, na.rm = TRUE)

age5_acid <- c(sum(age_5_acid, na.rm = TRUE), sum(age_5_homi, na.rm = TRUE), sum(age_5_suic, na.rm = TRUE))

barplot(age5_acid, names.arg = label_obito, xlab = "Tipo de morte", ylab = "Quantidade", main = "Mortes entre 81-99 anos")


# 100 >

age_6_acid <- (tipo_acidente & age6)
sum(age_6_acid, na.rm = TRUE)

age_6_homi <- (tipo_homicidio & age6)
sum(age_6_homi, na.rm = TRUE)

age_6_outros <- (tipo_outros & age6)
sum(age_6_outros, na.rm = TRUE)

age_6_suic <- (tipo_suicidio & age6)
sum(age_6_suic, na.rm = TRUE)

age6_acid <- c(sum(age_6_acid, na.rm = TRUE), sum(age_6_homi, na.rm = TRUE), sum(age_6_suic, na.rm = TRUE))

barplot(age6_acid, names.arg = label_obito, xlab = "Tipo de morte", ylab = "Quantidade", main = "Mortes entre pessoas com mais de 100 anos")
# Poucos dados 100 > 
