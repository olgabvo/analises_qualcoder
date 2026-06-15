### Juntando dados QualCoder 

### Limpando ambiente
rm(list = ls())

### Baixando pacotes ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(janitor)
library(rio)
library(stringdist)
library(writexl)
library(scales)

### Baixando dados --------------------------------------------------------------

# Pasta local
pasta_local <- "C:/Users/olgab/Documents/LEGO III"
pasta_local <- "C:/Users/olgab/Documents/LEGO III/GF_1a11_tudo"

# Comando para o R usar como referencia a pasta local
setwd(pasta_local)

## Baixando (tabelas) 

# 11 Grupos Focais retirados diretamente do QualCoder
# Cada um com as 11 categorias e 59 códigos
# Não estão modificados ainda

grupo1 <- import("GF1_tudo.csv")
grupo2 <- import("GF2_tudo.csv")
grupo3 <- import("GF3_tudo.csv")
grupo4 <- import("GF4_tudo.csv")
grupo5 <- import("GF5_tudo.csv")
grupo6 <- import("GF6_tudo.csv")
grupo7 <- import("GF7_tudo.csv")
grupo8 <- import("GF8_tudo.csv")
grupo9 <- import("GF9_tudo.csv")
grupo10 <- import("GF10_tudo.csv")
grupo11 <- import("GF11_tudo.csv")

### Modificando tabelas (uma a uma) ---------------------------------------------

## Os códigos (Codename) estão na ordem em que aparecem POR codename (group_by)
## Exemplo: "quem é o rico" está enumerado na ordem crescente, "mérito" está enumerado
# na ordem crescente e assim por diante. Isso vai ser importante pro ICR (inter-coder reability)
# quando separmos os códigos por participantes 

# 1
grupo1_mod <- grupo1 %>% 
  group_by(Codename, Category) %>% 
  mutate("ordem_cod" = row_number()) %>% 
  ungroup()

# 2
grupo2_mod <- grupo2 %>% 
  group_by(Codename, Category) %>% 
  mutate("ordem_cod" = row_number()) %>% 
  ungroup()

#3
grupo3_mod <- grupo3 %>% 
  group_by(Codename, Category) %>% 
  mutate("ordem_cod" = row_number()) %>% 
  ungroup()

#4
grupo4_mod <- grupo4 %>% 
  group_by(Codename, Category) %>% 
  mutate("ordem_cod" = row_number()) %>% 
  ungroup()

#5
grupo5_mod <- grupo5 %>% 
  group_by(Codename, Category) %>% 
  mutate("ordem_cod" = row_number()) %>% 
  ungroup()

#6
grupo6_mod <- grupo6 %>% 
  group_by(Codename, Category) %>% 
  mutate("ordem_cod" = row_number()) %>% 
  ungroup()

#7
grupo7_mod <- grupo7 %>% 
  group_by(Codename, Category) %>% 
  mutate("ordem_cod" = row_number()) %>% 
  ungroup()

#8
grupo8_mod <- grupo8 %>% 
  group_by(Codename, Category) %>% 
  mutate("ordem_cod" = row_number()) %>% 
  ungroup()

#9
grupo9_mod <- grupo9 %>% 
  group_by(Codename, Category) %>% 
  mutate("ordem_cod" = row_number()) %>% 
  ungroup()

#10
grupo10_mod <- grupo10 %>% 
  group_by(Codename, Category) %>% 
  mutate("ordem_cod" = row_number()) %>% 
  ungroup()

#11
grupo11_mod <- grupo11 %>% 
  group_by(Codename, Category) %>% 
  mutate("ordem_cod" = row_number()) %>% 
  ungroup()


## Apagando objetos desnecessários ----------------------------------------------
rm(grupo1,
   grupo2, 
   grupo3, 
   grupo4,
   grupo5,
   grupo6,
   grupo7,
   grupo8,
   grupo9,
   grupo10, 
   grupo11)

### Criação do objeto: grupo_juntos --------------------------------------------

# Juntar todos os objetos em um 
grupo_todos <- grupo1_mod %>% 
  rbind(grupo1_mod,
        grupo2_mod,
        grupo3_mod,
        grupo4_mod,
        grupo5_mod,
        grupo6_mod,
        grupo7_mod,
        grupo8_mod,
        grupo9_mod,
        grupo10_mod,
        grupo11_mod) %>% 
  select(File, 
         Category,
         Codename, 
         ordem_cod,
         Coded,
         everything())

## Apagando objetos desnecessários ----------------------------------------------
rm(grupo1_mod,
   grupo2_mod, 
   grupo3_mod, 
   grupo4_mod,
   grupo5_mod,
   grupo6_mod,
   grupo7_mod,
   grupo8_mod,
   grupo9_mod,
   grupo10_mod, 
   grupo11_mod)

## Resultado: grupo_todos 

# 4747 obs. 
# 8 variaveis
colnames(grupo_todos)

### Juntando com grupos posicionais --------------------------------------------
## Adicionando posição no grupo todos

# Baixando  
posic <- import("sql_report.csv")

## O id é o mesmo, só precisa ser renomeado 
## Cada um dos códigos, de cada grupo focal (de 1 a 11) vão aparecer na ordem em
## que foram codificados (quem é rico 1 - quem é o rico 2 - causa da pobreza 3, etc)

grupo_todos <- grupo_todos %>%
  mutate(ctid = str_remove(Id, "ctid:") %>% 
           as.integer()) %>% 
  inner_join(
    posic,
    by = "ctid") 

## Excluindo variáveis não relevantes e colocando em ordem 
grupo_todos <- grupo_todos %>% 
  select(-"avid", 
         -"important", 
         -"memo",
         -"owner",
         -"seltext", 
         -"Coded_Memo", 
         -"date") %>% 
  arrange("File", "pos0")

### Apagando posic (não vai mais ser usado porque já está dentro de grupo_todos)
rm(posic)

### TESTANDO A ORDEM -----------------------------------------------------------
grupo_todos %>% 
  filter(File == "Grupo 2 - Pobres - Brancos - 24-05-2024.docx") %>% 
  arrange(pos0) %>% 
  view()

### MAPA DE CALOR PELO CLAUDE

library(ggplot2)
library(dplyr)

# Filtrar apenas o File 2 (ajuste o nome conforme seu banco)
grupo_grafico_2 <- grupo_todos %>%
  filter(File == "Grupo 2 - Pobres - Brancos - 24-05-2024.docx")  # ajuste o valor conforme aparece no seu banco

# Criar ordem dos codenames conforme primeira aparição (por pos0)
ordem_codenames <- grupo_grafico_2 %>%
  arrange(pos0) %>%
  distinct(Codename) %>%
  pull(Codename)

# Criar coluna de ordem de ocorrência geral
grupo_grafico_2 <- grupo_grafico_2 %>%
  arrange(pos0) %>%
  mutate(
    ordem_ocorrencia = row_number(),
    Codename = factor(Codename, levels = rev(ordem_codenames))  # rev() para o primeiro aparecer no topo
  )

# Gráfico
ggplot(grupo_grafico_2, aes(x = ordem_ocorrencia, y = Codename, color = Codename)) +
  geom_point(size = 4, shape = 15) +  # shape 15 = quadrado, visual de "mapa de calor"
  scale_color_manual(values = colorRampPalette(
    c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00",
      "#a65628","#f781bf","#999999","#66c2a5","#fc8d62",
      "#8da0cb","#e78ac3","#a6d854","#ffd92f","#e5c494")
  )(length(unique(grupo_grafico_2$Codename)))) +
  labs(
    title = "Mapa de Ocorrências — File 2",
    x = "Ordem de Ocorrência",
    y = "Código (Codename)",
    color = "Codename"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",  # cores já identificam no eixo Y
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey90"),
    axis.text.y = element_text(size = 9)
  )

### OPÇÃO 2 --------------------------------------------------------------------

library(ggplot2)
library(dplyr)

# Filtrar File 2
grupo_grafico_2 <- grupo_todos %>%
  filter(File == "Grupo 2 - Pobres - Brancos - 24-05-2024.docx")%>%   # ajuste conforme seu banco
  arrange(pos0) %>%
  mutate(ordem_ocorrencia = row_number())

# Ordem dos codenames por primeira aparição dentro de cada Category
grupo_grafico_2 <- grupo_grafico_2 %>%
  group_by(Codename) %>%
  mutate(primeira_aparicao = min(pos0)) %>%
  ungroup() %>%
  arrange(Category, primeira_aparicao) %>%
  mutate(Codename = factor(Codename, levels = unique(Codename)))

# Gráfico
ggplot(grupo_grafico_2, aes(x = ordem_ocorrencia, y = Codename, color = Codename)) +
  geom_point(size = 3, shape = 15) +
  facet_wrap(
    ~ Category,
    scales = "free_y",   # cada painel só mostra os códigos daquela categoria
    ncol = 2             # 2 colunas de painéis — ajuste para 3 se preferir
  ) +
  scale_color_manual(values = colorRampPalette(
    c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00",
      "#a65628","#f781bf","#66c2a5","#fc8d62",
      "#8da0cb","#e78ac3","#a6d854","#ffd92f","#e5c494",
      "#b3b3b3","#1b9e77","#d95f02","#7570b3","#e7298a")
  )(length(unique(grupo_grafico_2$Codename)))) +
  labs(
    title = "Ocorrências por Código — File 2",
    subtitle = "Cada painel representa uma Category | Eixo X = ordem de ocorrência na conversa",
    x = "Ordem de Ocorrência",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10, color = "white"),
    strip.background = element_rect(fill = "grey30", color = NA),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey95"),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(1, "lines")
  )


### OPÇÃO 3 --------------------------------------------------------------------

library(ggplot2)
library(dplyr)

# Filtrar File 2
grupo_grafico_2 <- grupo_todos %>%
  filter(File == "Grupo 2 - Pobres - Brancos - 24-05-2024.docx") %>%  # ajuste conforme seu banco
  arrange(pos0)

# Definir número de blocos (ajuste conforme preferir)
n_blocos <- 20

# Criar blocos de tempo baseados em pos0
grupo_grafico_2 <- grupo_grafico_2 %>%
  mutate(
    bloco = cut(pos0,
                breaks = n_blocos,
                labels = 1:n_blocos,
                include.lowest = TRUE)
  )

# Ordem dos codenames por primeira aparição
ordem_codenames <- grupo_grafico_2 %>%
  group_by(Codename) %>%
  summarise(primeira = min(pos0)) %>%
  arrange(primeira) %>%
  pull(Codename)

# Contar ocorrências por bloco e codename
df_heat <- grupo_grafico_2 %>%
  group_by(bloco, Codename) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(Codename = factor(Codename, levels = rev(ordem_codenames)))

# Gráfico
ggplot(df_heat, aes(x = bloco, y = Codename, fill = n)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(
    low = "#fff7bc",
    high = "#d73027",
    name = "Ocorrências"
  ) +
  labs(
    title = "Heatmap de Ocorrências — File 2",
    subtitle = paste0("Conversa dividida em ", n_blocos, " blocos de tempo | Intensidade = nº de ocorrências por bloco"),
    x = "Bloco de Tempo (início → fim da conversa)",
    y = "Código (Codename)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 9),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.subtitle = element_text(size = 9, color = "grey40")
  )

# Salvar em alta resolução
ggsave("heatmap_file2.png", width = 14, height = 12, dpi = 300)

### OPÇÃO 4 --------------------------------------------------------------------

library(ggplot2)
library(dplyr)

# Filtrar File 2
grupo_grafico_2 <- grupo_todos %>%
  filter(File == "Grupo 2 - Pobres - Brancos - 24-05-2024.docx") %>%  # ajuste conforme seu banco
  arrange(pos0) %>%
  mutate(ordem_ocorrencia = row_number())

# Ordem dos codenames agrupados por categoria e por primeira aparição dentro dela
ordem_codenames <- grupo_grafico_2 %>%
  group_by(Category, Codename) %>%
  summarise(primeira = min(pos0), .groups = "drop") %>%
  arrange(Category, primeira) %>%
  pull(Codename)

grupo_grafico_2 <- grupo_grafico_2 %>%
  mutate(Codename = factor(Codename, levels = rev(ordem_codenames)))

# -------------------------------------------------------
# Paleta: cada categoria tem uma família de tons
# Ajuste os nomes das categorias conforme seu banco!
# -------------------------------------------------------
categorias <- unique(grupo_grafico_2$Category) %>% sort()

paletas <- list(
  c("#fff176","#f9a825","#f57f17","#e65100","#bf360c"),  # tons de amarelo/laranja
  c("#c8e6c9","#66bb6a","#2e7d32","#1b5e20"),            # tons de verde
  c("#bbdefb","#42a5f5","#1565c0","#0d47a1"),            # tons de azul
  c("#f8bbd0","#ec407a","#880e4f"),                      # tons de rosa
  c("#e1bee7","#ab47bc","#4a148c"),                      # tons de roxo
  c("#ffe0b2","#ff7043","#bf360c"),                      # tons de laranja
  c("#b2dfdb","#26a69a","#004d40"),                      # tons de teal
  c("#f5f5f5","#bdbdbd","#424242"),                      # tons de cinza
  c("#dcedc8","#aed581","#558b2f"),                      # tons de verde-limão
  c("#fff9c4","#fff176","#f9a825","#ff6f00"),            # tons de amarelo forte
  c("#fce4ec","#f48fb1","#c2185b")                       # tons de pink
)

# Associar cada codename à sua cor dentro da família da categoria
cor_por_codename <- grupo_grafico_2 %>%
  group_by(Category, Codename) %>%
  summarise(primeira = min(pos0), .groups = "drop") %>%
  arrange(Category, primeira) %>%
  group_by(Category) %>%
  mutate(
    idx_cat = match(first(Category), categorias),
    paleta_cat = list(paletas[[idx_cat]]),
    n_codigos = n(),
    idx_codigo = row_number(),

### PARA ICR: Criação do objeto grupos_todos_separados--------------------------

# Separando para mais linhas os interlocutores 

grupo_todos_separado <- grupo_todos1 %>%
  separate_rows(
    # Separar cada vez que um interlocutor fala
    Coded,
    sep = "\\s*(?=P\\d+:|M:)\\s*") %>%
  
  # tirar todos os NA do Coded da tabela
  filter(!is.na(Coded)) %>%
  
  # Tirar todos os espaços vazios do Coded da tabela
  filter(str_squish(Coded) != "")%>%
  
  mutate( # Limpar o Coded
    Coded = stringr::str_squish(Coded),
    
    # Extrair o interlocutor
    interlocutor = str_extract(Coded, "^[A-Z]+\\d*|M")) %>%
  
  mutate(
        # Criar nova variável de qual Grupo pertence
        grupo_num = str_extract(File, "\\d+"),
    
        # Criar nova variável de qual interlocutor que é
        interlocutor = str_extract(Coded, "^[A-Z]+\\d*|M"),
    
        # Criar nova variável para interlocutor final (G1_P1) 
        interlocutor_final = paste0("G",
                             grupo_num,
                             "_",
                             interlocutor)) %>% 
  select( # Reorganizando a ordem
         File, 
         Category, 
         Codename,
         ordem_cod,
         interlocutor_final,
         interlocutor, 
         Coded, 
         everything(), 
         -grupo_num # Apagando variável desnecessária da tabela
         )%>% 
  
  mutate(interlocutor = if_else(interlocutor == "P9", "PN",interlocutor),
         interlocutor = if_else(interlocutor == "E", "M",interlocutor),
         interlocutor = if_else(interlocutor == "A", "M",interlocutor)) %>% 
  mutate(interlocutor_final = if_else(interlocutor_final == "G7_P9", "G7_PN",interlocutor_final),
         interlocutor_final = if_else(interlocutor_final == "G6_A", "G6_M",interlocutor_final),
         interlocutor_final = if_else(interlocutor_final == "G2_E", "G2_M",interlocutor_final),
         interlocutor_final = if_else(interlocutor_final == "G9_P8", "G9_PN",interlocutor_final))

# Apagar A e E porque eram Moderadores
# Mudar o P9 para PN (porque não tem nenhum participante P9)


####### Brincando --------------------------------------------------------------
grupo_todos_separado %>% 
  tabyl(ordem_cod)


tabyl(grupo_todos_separado, interlocutor_final)

grupo_todos_separado %>% 
  filter(Category == "3 Desigualdades", 
         File == "Grupo 1 - Pobres - Pretos e Pardos - 23-05-2024.docx") %>% 
  tabyl(Codename, interlocutor) %>% 
  arrange(Codename)

grupo_todos %>% 
  filter(Category == "3 Desigualdades", 
         File == "Grupo 1 - Pobres - Pretos e Pardos - 23-05-2024.docx") %>% 
  tabyl(Codename) %>%  
  arrange(Codename)

## Trocar interlocutor para participante

grupo_todos_separado %>% 
  tabyl(File)

grupo_todos %>% 
  view()

grupo_todos_separado %>% 
  filter(File == "Grupo 1 - Pobres - Pretos e Pardos - 23-05-2024.docx", 
         interlocutor == "P4", 
         Category == "3 Desigualdades") %>% 
  select(-Coded) %>% 
  arrange(ordem_cod) %>% 
  tabyl(ordem_cod, Codename)
  
grupo_todos_separado %>% 
  tabyl(interlocutor)

glimpse(grupo_todos_separado)
  
tabyl (grupo_todos, File)

colnames(grupo_todos)
glimpse(grupo_todos_separado)

any(is.na(grupo_todos_separado$Coded_Memo))
all(is.na(grupo_todos_separado$Coded_Memo))
sum(!is.na(grupo_todos_separado$Coded_Memo))

### Tentando entender ordem_cod 

grupo_todos %>%
  filter(Category == "00 Riqueza") %>%
  filter(File == "Grupo 11 - Pobres - Misto - DIreita - 25-11-2024.docx") %>%
  select(Category, 
         Codename, 
         Id, 
         ordem_cod, Coded) %>%
  arrange(Id) %>% 
  view()

#### Como que os dados aparecem de acordo com o tempo: gráficos!! -------

# Gráfico 1
grupo_todos1 %>% 
  filter(File == "Grupo 11 - Pobres - Misto - DIreita - 25-11-2024.docx", 
         Category == "2 Fontes da Riqueza") %>% 
  arrange(pos0) %>% 
  ggplot(    aes(x = pos0,
             fill = Codename)) +
  
  geom_density(alpha = 0.5) +
  
  facet_wrap(~Codename,
             ncol = 1,
             scales = "free_y") +
  
  scale_x_continuous(labels = comma) +
  
  theme_minimal(base_size = 14)
  
# Gráfico 1 b 

grupo_todos1 %>% 
  filter(File == "Grupo 11 - Pobres - Misto - DIreita - 25-11-2024.docx", 
         Category == "2 Fontes da Riqueza") %>% 
  ggplot(aes(x = pos0,
             fill = Codename,
             color = Codename)) +
  
  geom_density(alpha = 0.4,
               linewidth = 0.8,
               adjust = 2.5) +
  
  facet_wrap(~Codename,
             ncol = 1,
             scales = "free_y") +
  
  scale_x_continuous(labels = scales::comma) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank())

# Gráfico 2
grupo_todos_separado %>% 
  filter(File == "Grupo 11 - Pobres - Misto - DIreita - 25-11-2024.docx", 
         Category == "2 Fontes da Riqueza") %>% 
  arrange(pos0) %>% 
  ggplot(aes(x = pos0,
             y = Codename,
             fill = Codename)) +
  
  geom_density(alpha = 0.6) +
  
  scale_x_continuous(labels = scales::comma) +
  
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Gráfico 3
grupo_todos_separado %>% 
  filter(File == "Grupo 11 - Pobres - Misto - DIreita - 25-11-2024.docx", 
         Category == "2 Fontes da Riqueza") %>% 
  arrange(pos0) %>% 
  ggplot(aes(x = pos0, y = Codename)) +
  geom_bin2d(bins = 50) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal(base_size = 14)
  
# Gráfico 4

grupo_todos_separado %>% 
  filter(File == "Grupo 11 - Pobres - Misto - DIreita - 25-11-2024.docx", 
         Category == "2 Fontes da Riqueza") %>% 
  arrange(pos0) %>%
  ggplot(  aes(x = pos0,
           y = Codename,
           color = Codename)) +
  
  geom_point(alpha = 0.6,
             size = 1.6,
             position = position_jitter(height = 0.15)) +
  
  scale_x_continuous(labels = comma) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  
  labs(
    x = "Posição (ordem no texto)",
    y = "Codename")

# Gráfico 5

grupo_todos_separado %>% 
  filter(File == "Grupo 11 - Pobres - Misto - DIreita - 25-11-2024.docx", 
         Category == "2 Fontes da Riqueza") %>% 
  arrange(pos0) %>%
  ggplot(  aes(x = pos0,
           y = Codename,
           color = Codename)) +
  
  geom_point(alpha = 0.5,
             size = 1.3) +
  
  scale_x_continuous(labels = scales::comma) +
  
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

### avaliando

grupo_todos_separado %>% 
  filter(File == "Grupo 11 - Pobres - Misto - DIreita - 25-11-2024.docx", 
         Codename == "25 Exploração",
         pos0 == 22580) %>% 
  arrange(pos0) %>% 
  select(Coded) %>% 
  head(20)




### Salvando tabela em csv -----------------------------------------------------

write_csv(grupo_todos_separado, "dados_qc_trabalhados.csv")

## Conferindo onde está o arquivo

getwd()

#### Tentando intercoder_reability (ICR) ---------------------------------------------

### Ingrid: FG06 e FG11

## Baixando dados Ingrid

# Codificações Ingrid
cod_ing <- import("cod_ingrid_FG06_e_FG11.csv")

## Vendo variáveis

colnames(cod_ing)
colnames(grupo6_mod)
colnames(grupo11_mod)
count(grupo6_mod, Category)


##### PASSO 1 -----
## Grupo 6
## Referência

# grupo6_mod
a_ref_6 <- grupo6_mod %>% 
  filter(Category == "99 Referências") %>% 
  select(-ordem_cod) 

# cod_ingrid
b_ref_6 <- cod_ing %>% 
  filter(File == "Grupo 6 - Classe Média - Misto - 05-11-2024.docx",
         Category == "8 Referências") 

## Grupo 6
## Abrangência

# grupo6_mod
a_abr_6 <- grupo6_mod %>% 
  filter(Category == "99 Abrangência") %>% 
  select(-ordem_cod) 

# cod_ingrid
b_abr_6 <- cod_ing %>% 
  filter(File == "Grupo 6 - Classe Média - Misto - 05-11-2024.docx",
         Category == "9 Abrangência") 

## Grupo 11
## Referência 

# grupo11_mod
a_ref_11 <- grupo11_mod %>% 
  filter(Category == "99 Referências") %>% 
  select(-ordem_cod) 

# cod_ingrid
b_ref_11 <- cod_ing %>% 
  filter(File == "Grupo 11 - Pobres - Misto - DIreita - 25-11-2024.docx",
         Category == "8 Referências") 

## Grupo 11
## Abrangência

# grupo11_mod
a_abr_11 <- grupo11_mod %>% 
  filter(Category == "99 Abrangência") %>% 
  select(-ordem_cod) 

# cod_ingrid
b_abr_11 <- cod_ing %>% 
  filter(File == "Grupo 11 - Pobres - Misto - DIreita - 25-11-2024.docx",
         Category == "9 Abrangência") 

#### OUTRA COISA -----
# Testando
cod_ing %>% 
  tabyl(File)

grupo6_mod %>% 
  tabyl(Category)

cod_ing %>% 
  filter(File == "Grupo 6 - Classe Média - Misto - 05-11-2024.docx") %>% 
  tabyl(Category)

# 1) Trocar nome de categoria 
# 2) Tem no grupo6_mod 35 linhas e tem cod_ing 43 linhas

## tentando pensar!!!

# Legenda: 
# a = grupo_mod
# b = cod_ingrid

# ref = referência
# abr = abrangência

# 6 = grupo 6
# 11 = grupo 11

#### Passo a passo

### Passo 1: padronização 
## Criação da função para não ter que repetir
## Limpar texto: criar função para agir quando eu quiser

limpar <- function(x){
  x %>%
    str_to_lower() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish()
}


### Passo 2: Criar objetos da versão limpa ----
## De cada objeto

#### 6

# a_ref_6

a_ref_6_l <- a_ref_6 %>% 
  mutate(texto_limpo = limpar(Coded))

# b_ref_6

b_ref_6_l <- b_ref_6 %>% 
  mutate(texto_limpo = limpar(Coded))

### Abrangência
# a_abr_6

a_abr_6_l <- a_abr_6 %>% 
  mutate(texto_limpo = limpar(Coded))

# b_abr_6

b_abr_6_l <- b_abr_6 %>% 
  mutate(texto_limpo = limpar(Coded))

#### 11

# a_ref_11

a_ref_11_l <- a_ref_11 %>% 
  mutate(texto_limpo = limpar(Coded))

# b_ref_11

b_ref_11_l <- b_ref_11 %>% 
  mutate(texto_limpo = limpar(Coded))

### Abrangência
# a_abr_11

a_abr_11_l <- a_abr_11 %>% 
  mutate(texto_limpo = limpar(Coded))

# b_abr_11

b_abr_11_l <- b_abr_11 %>% 
  mutate(texto_limpo = limpar(Coded))


#### Novo passo a passo: ACHO QUE ESTÁ DANDO CERTO -----------------------------

# Limpar ambiente (vou ficar louca)

### PASSO 1: separar matches "exatos"

# Antes de considerar erro, trocar nome dos Codenames, que não estão idênticos

tabyl(a_ref_6_l, Codename)
tabyl(b_ref_6_l, Codename)

#### PASSO 3 ----

# ref 6
a_ref_6_l <- a_ref_6_l %>% 
  mutate(Codename = (case_when(Codename == "995 Experiência" ~ "Experiência",
                               Codename == "996 Factual" ~ "Factual",
                               Codename == "997a Mídia tradicional" ~ "Mídia tradicional",
                               Codename == "997b Mídias sociais" ~ "Mídias sociais",
                               Codename == "998 Pressuposto / suposição" ~ "Pressuposto/suposição",
                               Codename == "999 Terceiros" ~ "Terceiros"))) 

# abr 6

a_abr_6_l <- a_abr_6_l %>% 
  mutate(Codename = (case_when(Codename == "990 Futuro" ~ "Futuro",
                               Codename == "991 Passado" ~ "Passado",
                               Codename == "992 Global" ~ "Global",
                               Codename == "993 Local" ~ "Local",
                               Codename == "994 Nacional" ~ "Nacional"))) 


# ref 11

a_ref_11_l <- a_ref_11_l %>% 
  mutate(Codename = (case_when(Codename == "995 Experiência" ~ "Experiência",
                               Codename == "996 Factual" ~ "Factual",
                               Codename == "997a Mídia tradicional" ~ "Mídia tradicional",
                               Codename == "997b Mídias sociais" ~ "Mídias sociais",
                               Codename == "998 Pressuposto / suposição" ~ "Pressuposto/suposição",
                               Codename == "999 Terceiros" ~ "Terceiros"))) 

# abr 11

a_abr_11_l <- a_abr_11_l %>% 
  mutate(Codename = (case_when(Codename == "990 Futuro" ~ "Futuro",
                               Codename == "991 Passado" ~ "Passado",
                               Codename == "992 Global" ~ "Global",
                               Codename == "993 Local" ~ "Local",
                               Codename == "994 Nacional" ~ "Nacional"))) 

### ERRO COD DA ingrid ??? -----

cod_ing %>% 
  filter(Category == "9 Abrangência") %>% 
  tabyl(Codename)


colnames(b_ref_6_l)
tabyl(b_abr_11_l, Codename)
tabyl(a_abr_11_l, Codename)

# limpando ambiente 
rm(a_ref_6,  b_ref_6,
   a_ref_11, b_ref_11, 
   a_abr_6,  b_abr_6, 
   a_abr_11, b_abr_11) 

rm(grupo_todos_separado)


# 2: PRIMEIRO PASSO PARA ICR -----

## Ref 6
icr_6_ref <- a_ref_6_l %>%
  inner_join(
    b_ref_6_l,
    by = "Codename",
    suffix = c("_a", "_b")) %>% 
  mutate(match_exato = Coded_a == Coded_b,
         match_contido = str_detect(Coded_a, fixed(Coded_b)) |
         str_detect(Coded_b, fixed(Coded_a))) %>% 
  filter(match_exato | match_contido)

## Abr 6

icr_6_abr <- a_abr_6_l %>%
  inner_join(
    b_abr_6_l,
    by = "Codename",
    suffix = c("_a", "_b")) %>% 
  mutate(match_exato = Coded_a == Coded_b,
         match_contido = str_detect(Coded_a, fixed(Coded_b)) |
           str_detect(Coded_b, fixed(Coded_a))) %>%  
  filter(match_exato | match_contido)


## Ref 11

icr_11_ref <- a_ref_11_l %>%
  inner_join(
    b_ref_11_l,
    by = "Codename",
    suffix = c("_a", "_b")) %>% 
  mutate(match_exato = Coded_a == Coded_b,
         match_contido = str_detect(Coded_a, fixed(Coded_b)) |
           str_detect(Coded_b, fixed(Coded_a))) %>% 
  filter(match_exato | match_contido)

## Abr 11

icr_11_abr <- a_abr_11_l %>%
  inner_join(
    b_abr_11_l,
    by = "Codename",
    suffix = c("_a", "_b")) %>% 
  mutate(match_exato = Coded_a == Coded_b,
         match_contido = str_detect(Coded_a, fixed(Coded_b)) |
           str_detect(Coded_b, fixed(Coded_a))) %>% 
  filter(match_exato | match_contido)


### Apagando o que não precisa ----
rm(a_abr_6_l, b_abr_6_l, 
   a_ref_6_l, b_ref_6_l,
   a_abr_11_l,b_abr_11_l,
   a_ref_11_l,b_ref_11_l)


#### Juntando bases de dados para ICR

icr_todos <- icr_6_abr %>% 
  rbind(icr_6_ref, 
        icr_11_abr,
        icr_11_ref)


sum(nrow(icr_11_abr),nrow(icr_11_ref),
    nrow(icr_6_abr), nrow(icr_6_ref))

## teste ref 6

# ref a
nrow(a_ref_6_l)
# 35

# ref b
nrow(b_ref_6_l)
# 43

# contido a + b
nrow(icr_6_ref)
# 25

# não contido a + b 
sum(nrow(a_ref_6_l), nrow(b_ref_6_l))
nrow(a_ref_6_l)-nrow(icr_6_ref) + nrow(b_ref_6_l)-nrow(icr_6_ref)
# 28

# contido + não contido = total
nrow(icr_6_ref)+nrow(a_ref_6_l)-nrow(icr_6_ref) + nrow(b_ref_6_l)-nrow(icr_6_ref)

# proporção = contido / total
nrow(icr_6_ref)/(nrow(icr_6_ref)+nrow(a_ref_6_l)-nrow(icr_6_ref) + nrow(b_ref_6_l)-nrow(icr_6_ref))

# Fórmula

## Ref 11
nrow(icr_11_ref)/(nrow(icr_11_ref)+nrow(a_ref_11_l)-nrow(icr_11_ref) + nrow(b_ref_11_l)-nrow(icr_11_ref))

## Abr 6
nrow(icr_6_abr)/(nrow(icr_6_abr)+nrow(a_abr_6_l)-nrow(icr_6_abr) + nrow(b_abr_6_l)-nrow(icr_6_abr))

## Abr 11
nrow(icr_11_abr)/(nrow(icr_11_abr)+nrow(a_abr_11_l)-nrow(icr_11_abr) + nrow(b_abr_11_l)-nrow(icr_11_abr))

prop_icr <- tibble(
  Grupo = c("Grupo 6", "Grupo 11"),
  Abrangencia (NIED) = c(nrow(a_abr_6_l), nrow(a_abr_11_l)),
  
  Abrangencia (Ingrid) = c(nrow(b_abr_6_l), nrow(b_abr_11_l)),
  
  Abrangencia (contidos) = c(icr_6_abr, icr_11_abr),
  
  Abrangencia (prop) = c(
    nrow(icr_6_abr)/(nrow(icr_6_abr)+nrow(a_abr_6_l)-nrow(icr_6_abr)+ nrow(b_abr_6_l)-nrow(icr_6_abr)), 
    nrow(icr_11_abr)/(nrow(icr_11_abr)+nrow(a_abr_11_l)-nrow(icr_11_abr) + nrow(b_abr_11_l)-nrow(icr_11_abr))),
  
  Referencia (NIED) = c(nrow(a_ref_6_l), nrow(a_ref_11_l)),
  
  Abrangencia (Ingrid) = c(nrow(b_ref_6_l), nrow(b_ref_11_l)),
  
  Abrangencia (contidos) = c(icr_6_ref, icr_11_ref),
  
  Referencia (prop) = c(
    nrow(icr_6_ref)/(nrow(icr_6_ref)+nrow(a_ref_6_l)-nrow(icr_6_ref) + nrow(b_ref_6_l)-nrow(icr_6_ref)), 
    nrow(icr_11_ref)/(nrow(icr_11_ref)+nrow(a_ref_11_l)-nrow(icr_11_ref) + nrow(b_ref_11_l)-nrow(icr_11_ref))
    )
  )

prop_icr <- tibble(
  Grupo = c("Grupo 6", "Grupo 11"),
  
  `Abrangencia (NIED)` = c(nrow(a_abr_6_l), nrow(a_abr_11_l)),
  
  `Abrangencia (Ingrid)` = c(nrow(b_abr_6_l), nrow(b_abr_11_l)),
  
  `Abrangencia (contidos)` = c(nrow(icr_6_abr), nrow(icr_11_abr)),
  
  `Abrangencia (prop)` = c(
    nrow(icr_6_abr) /
      (nrow(icr_6_abr) +
         nrow(a_abr_6_l) - nrow(icr_6_abr) +
         nrow(b_abr_6_l) - nrow(icr_6_abr)),
    
    nrow(icr_11_abr) /
      (nrow(icr_11_abr) +
         nrow(a_abr_11_l) - nrow(icr_11_abr) +
         nrow(b_abr_11_l) - nrow(icr_11_abr))
  ),
  
  `Referencia (NIED)` = c(nrow(a_ref_6_l), nrow(a_ref_11_l)),
  
  `Referencia (Ingrid)` = c(nrow(b_ref_6_l), nrow(b_ref_11_l)),
  
  `Referencia (contidos)` = c(nrow(icr_6_ref), nrow(icr_11_ref)),
  
  `Referencia (prop)` = c(
    nrow(icr_6_ref) /
      (nrow(icr_6_ref) +
         nrow(a_ref_6_l) - nrow(icr_6_ref) +
         nrow(b_ref_6_l) - nrow(icr_6_ref)),
    
    nrow(icr_11_ref) /
      (nrow(icr_11_ref) +
         nrow(a_ref_11_l) - nrow(icr_11_ref) +
         nrow(b_ref_11_l) - nrow(icr_11_ref))
  )
)

prop_icr %>% 
  view()


###### Salvando para Germano icr_todos ------

write_csv(icr_todos, "para_icr_6_11.csv")
write_csv(prop_icr, "para_prop_icr_6_11.csv")

write_csv(icr_11_abr, "contidos_11_abr.csv")
write_csv(icr_11_ref, "contidos_11_ref.csv")

write_csv(icr_6_abr, "contidos_6_abr.csv")
write_csv(icr_6_ref, "contidos_6_ref.csv")

### TENTANTO SEM FILTER 
teste3_exatos_semanticos <- a_ref_6_l %>%
  inner_join(
    b_ref_6_l,
    by = "Codename",
    suffix = c("_a", "_b")) %>% 
  mutate(match_exato = Coded_a == Coded_b,
         match_contido = str_detect(Coded_a, fixed(Coded_b)) |
           str_detect(Coded_b, fixed(Coded_a))) 

view(teste3_exatos_semanticos)
  


## Não 
exactos_semanticos <- a_ref_6_l %>%
  inner_join(
    b_ref_6_l,
    by = "Codename",
    suffix = c("_a", "_b")
  ) %>%
  mutate(
    match_exato = Coded_a == Coded_b,
    match_contido = str_detect(Coded_a, fixed(Coded_b)) |
      str_detect(Coded_b, fixed(Coded_a))
  ) %>%
  filter(match_exato | match_contido)

# Não 
exatos_semanticos <- a_ref_6_l %>%
  inner_join(
    b_ref_6_l,
    by = "Codename",
    suffix = c("_a", "_b")) %>%
  mutate(match_exato = Coded_a == Coded_b,
         match_contido = str_detect(Coded_a, fixed(Coded_b)) |
                        str_detect(Coded_b, fixed(Coded_a))) %>%
  filter(match_exato | (match_contido & stringsim(Coded_a, Coded_b) > 0.75))

# Só pra ver
teste1_exatos_semanticos %>% 
  select(Id_a, Id_b,
         Codename, Codename,
         texto_limpo_a, texto_limpo_b,
         match_exato, match_contido) %>% 
  view()

colnames(exatos_semanticos)

### INTERCODER REALIBILITY

## TESTE 2: CHAT GPT
teste2_exatos_semanticos <- a_ref_6_l %>%
  inner_join(
    b_ref_6_l,
    by = "Codename",
    suffix = c("_a", "_b")) %>%
  mutate(match_exato = Coded_a == Coded_b,
    
         match_contido = str_detect(Coded_a, fixed(Coded_b)) |
         str_detect(Coded_b, fixed(Coded_a)),
    
         sim = stringsim(Coded_a, Coded_b, method = "jw"))%>%
  mutate(intercoder_score = case_when(match_exato ~ 1,
                                      match_contido & sim >= 0.85 ~ 0.9,
                                      sim >= 0.85 ~ 0.8,
                                      sim >= 0.70 ~ 0.6,
                                      TRUE ~ 0))


### TESTE 2: NOSSO GERMANO E OLGA
teste2_exatos_semanticos <- a_ref_6_l %>%
  inner_join(
    b_ref_6_l,
    by = "Codename",
    suffix = c("_a", "_b")) %>%
  mutate(match_exato = Coded_a == Coded_b,
         
         match_contido = str_detect(Coded_a, fixed(Coded_b)) |
           str_detect(Coded_b, fixed(Coded_a)),
         
         sim = stringsim(Coded_a, Coded_b, method = "jw"))%>%
  mutate(intercoder_score = case_when(match_exato ~ 1,
                                      match_contido & sim >= 0.99 ~ 0.5,
                                      TRUE ~ 0))

teste2_exatos_semanticos_a <- teste2_exatos_semanticos %>% 
  summarise(
    intercoder_reliability = mean(intercoder_score, na.rm = TRUE)
  )

### PASSO 2: pegar o que NÃO casou

a_nao_match <- anti_join(a_ref_6_l, b_ref_6_l, by = "Codename")
b_nao_match <- anti_join(b_ref_6_l, a_ref_6_l, by = "Codename")

### PASSO 3: fuzzy matching só para os restos

### PASSO 4: similaridade textual

### PASSO 5: selecionar melhores matches


### Passo 3: ALTERAR: Gerar todas as combinações possíveis ----

combinacoes_ref <- a_ref_6_l %>% 
  crossing(b_ref_6_l, suffix = c("_a", "_b"))

colnames(a_ref_6_l)
colnames(b_ref_6_l)

b_ref_6_l %>% 
  select(-Coded, -texto_limpo, -Coded_Memo) %>% 
  tabyl(Codename)


a_ref_6_l %>% 
  select(Coded, texto_limpo) %>% 
  head(1)

# Tentativa 2

a_ref_6_l <- a_ref_6_l %>%
  rename_with(~ paste0(.x, "_a"))

b_ref_6_l <- b_ref_6_l %>%
  rename_with(~ paste0(.x, "_b"))

## Tem um problema seríssimo em não estar relacionado com o Codename!!!         

### Passo 7: O que não teve match fica separado 

### obs.: matching fuzzy 


#### Tentanto categorizar Família (a principio) --------------------------------

## Teste 
colnames(grupo_todos_separado)
colnames(grupo_todos)


grupo_todos

## Vendo umas coisas aí 

grupo_todos %>% 
  filter(Category == "2 Fontes da Riqueza") %>% 
  tabyl(Codename)


grupo_todos %>%
  filter(Codename == "23 Família") %>%
  unique() %>% 
  nrow()
  write_xlsx("familia_junta.xlsx")

  
tabyl(grupo_todos, File)
