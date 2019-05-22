

##  Save Data ##-------------------

# only consider date NA and 2019 
# get varience and 

library(xlsx)
wb <- createWorkbook()
sheet <- createSheet(wb, "Sheet1")
rows  <- createRow(sheet, rowIndex=1)    
cell.1 <- createCell(rows, colIndex=1)[[1,1]]     
setCellValue(cell.1, "Hello R!")
cellStyle1 <- CellStyle(wb) +
  Fill(backgroundColor="orange", foregroundColor="orange",
       pattern="SOLID_FOREGROUND") 
setCellStyle(cell.1, cellStyle1) 
# Then save the workbook 
saveWorkbook(wb, "filename.xlsx")
