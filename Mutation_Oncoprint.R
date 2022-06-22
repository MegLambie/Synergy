library(ComplexHeatmap)
library(circlize)

data <- read.csv("Mutation_22Q2_Public_subsetted.csv")
data1<-read.csv("Mutation_22Q2_Public_subsetted_1.csv", stringsAsFactors = FALSE, 
                row.names = 1)

column_split = rep(c("Resistant", "Sensitive"), c(6, 13))

get_type_fun = function(x) strsplit(x, ";")[[1]]

get_type_fun(data1[1,1])

col = c(MISSENSE = 'darkgreen', TRUNC = 'darkgreen', OTHER = 'darkgreen')

oncoPrint(data1,
          alter_fun = list(
            background = function(x, y, w, h) 
              grid.rect(x, y, w*0.9, h*0.9, gp = gpar(fill = "#CCCCCC", col = NA)),
            MISSENSE = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9, 
                                                      gp = gpar(fill = col["MISSENSE"], col = NA)),
            TRUNC = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9, 
                                                   gp = gpar(fill = col["TRUNC"], col = NA)),
            OTHER = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9, 
                                                   gp = gpar(fill = col["OTHER"], col = NA))),
          col = col, column_split = column_split,
          top_annotation = NULL, right_annotation = NULL, show_column_names = T,
          pct_side = "right", row_names_side = "left")


data2<-read.csv("Mutation_22Q2_Public_subsetted_2.csv", stringsAsFactors = FALSE, 
                row.names = 1)

column_split = rep(c("Resistant", "Sensitive"), c(6, 13))

get_type_fun = function(x) strsplit(x, ";")[[1]]

get_type_fun(data1[1,1])

col = c(MUT = 'darkgreen', WT = '#CCCCCC')

col = c(MISSENSE = 'darkred', TRUNC = 'darkblue', OTHER = 'darkgreen')

oncoPrint(data2,
          alter_fun = list(
            MUT = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9, 
                                                 gp = gpar(fill = col["MISSENSE"], col = NA)),
            WT = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9, 
                                                gp = gpar(fill = col["WT"], col = NA))),
          
          col = col, column_split = column_split,
          #alter_fun_is_vectorized = FALSE,
          top_annotation = NULL, right_annotation = NULL, show_column_names = T,
          pct_side = "right", row_names_side = "left")




res = data2[c(1,2,4, 5, 7, 9),1:6]
sen = data[c(1,2,4, 5, 7, 9),7:19]

ht1 = oncoPrint(res,
                alter_fun = list(
                  background = function(x, y, w, h) 
                    grid.rect(x, y, w*0.9, h*0.9, gp = gpar(fill = "#CCCCCC", col = NA)),
                  MISSENSE = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9, 
                                                            gp = gpar(fill = col["MISSENSE"], col = NA)),
                  TRUNC = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9, 
                                                         gp = gpar(fill = col["TRUNC"], col = NA)),
                  OTHER = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9, 
                                                         gp = gpar(fill = col["OTHER"], col = NA))),
                col = col, 
                top_annotation = NULL, right_annotation = NULL, show_column_names = T,
                column_title = 'Radioresistant', 
                pct_side = "right", row_names_side = "left")


ht2 = oncoPrint(sen,
                alter_fun = list(
                  background = function(x, y, w, h) 
                    grid.rect(x, y, w*0.9, h*0.9, gp = gpar(fill = "#CCCCCC", col = NA)),
                  MISSENSE = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9, 
                                                            gp = gpar(fill = col["MISSENSE"], col = NA)),
                  TRUNC = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9, 
                                                         gp = gpar(fill = col["TRUNC"], col = NA)),
                  OTHER = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9, 
                                                         gp = gpar(fill = col["OTHER"], col = NA))),
                col = col, 
                top_annotation = NULL, 
                #right_annotation = NULL, 
                show_column_names = T,
                column_title = 'Radiosensitive', 
                pct_side = "right", row_names_side = "left", show_row_names = F,
                show_heatmap_legend = F, right_annotation = ha)

ht_list = ht1 + ht2
onco<-draw(ht_list, ht_gap = unit(1, "cm"), column_title = "Cell line mutations by radiation response",
           show_heatmap_legend = T)


ha = rowAnnotation(Fisher = anno_text(fisher, location = 0.5, just = 'center',
                                      gp = gpar(fill = "darkgrey", col = "white", border = "black"),
                                      width = max_text_width(fisher)*1.5))




##Fishers exact test for each gene grouping

dat <- data.frame(
  "mutated" = c(1, 0),
  "WT" = c(5, 13),
  row.names = c("Resistant", "Sensitive"),
  stringsAsFactors = FALSE
)
colnames(dat) <- c("mutated", "WT")

dat



x<- fisher.test(dat)
fisher.pvals<-c(fisher.pvals, x$p.value)

fisher.pvals<-round(fisher.pvals, digits = 2)

fisher<-as.character(fisher.pvals)

dat <- data.frame(
  "mutated" = c(6, 12),
  "WT" = c(0, 1),
  row.names = c("Resistant", "Sensitive"),
  stringsAsFactors = FALSE
)
colnames(dat) <- c("mutated", "WT")

dat

?prop.test

p1 <- 4/6
n1 <- 6
p2 <- 5/13
n2 <- 13
p <- (n1 * p1 + n2 * p2)/ (n1 + n2)
z <- (p1 - p2) / sqrt(p * (1-p) * (1/n1 + 1/n2))
z
