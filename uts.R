data = c(12,4,8,7,12,4,4,4,8,3,10,7,6,6,3,6,12,11,12,4,12,4,6,10,5,5,4,5,7,8,9,1,6,4,3,6,10,5,5,6,6,7,9,7,5,4,4,6,11,5,11,10,5,9,6,4,4,9,12,8,8,8,7,7,9,10,9,7,8,4,6,7,4,7,12,9,3,8,4,8,7,3,11,3,7,7,8,9,6,7,8,4,9,10,10,6,8,9,7,4,10,7,11,8,9,6,9,7,6,9,11,8,8,7,8,9,9,8,7,8,8,9,8,10,3,12,7,10,9,9,11,9,12,5,8,8,9,12,12,9,10,6,9,8,10,11,10,7,12,10,9,10,10,11,8,8,9,10,8,8,5,9,9,8,10,9,8,7,10,7,9,7,7,10,7,7,7,7,8,10,9,7,12,9,8,8,8,8,7,6,7,12,10,7,10,5,11,8,11,5,10,10,12,6,11,10,9,9,8,10,4,5,5)
sort(data)

tabel = list(data = data,fi=)

n = length(data)
nmax = max(data)
nmin = min(data)
nmax
nmin
#menentukan jumlah kelas [K]
k = 1+(3.3*log10(n))
K= round(k)
K
#menetukan interval class [p]
p = (nmax-nmin)/K
p = ceiling(p)
p

frequensi = function(x,y,z){
  a =0
  for (i in 1:n){
    if(x[i]>=y&&x[i]<=z){
      a = a+1
    }
  }
  a
}
frek1= frequensi(data,36,40)
frek2= frequensi(data,41,45)
frek3= frequensi(data,46,50)
frek4= frequensi(data,51,55)
frek5= frequensi(data,56,60)
frek6= frequensi(data,61,65)
frek7= frequensi(data,66,70)
frek1
frek2
frek3
frek4
frek5
frek6
frek7
nilai_tengah <- list(38,43,48,53,58,63,68)

# mencari rata rata(mean)
rata= sum(data)/length(data)
rata

#mencari median data kelompok
median1= (36+40)/2
median2= (41+45)/2
median3= (46+50)/2
median4= (51+55)/2
median5= (56+60)/2
median6= (61 + 65)/2
median7= (66 + 70)/2
median6


#mencari Modus
tb <- 6.5 -0.5
d2 <- 155
d1 <- 97
p = 6
MODUS <- tb + (d1 / (d1+d2)) *p
MODUS

#mencai range
nilai_max = 12
nilai_min = 6
range = nilai_max - nilai_min
range

#simpangan rata-rata
fx1=frek1*((median1-rata)*-1)
fx2=frek2*((median2-rata)*-1)
fx3=frek3*((median3-rata)*-1)
fx4=frek4*(median4-rata)
fx5=frek5*(median5-rata)
fx6=frek6*(median6-rata)
fx7=frek7*(median7-rata)
fixi= fx1+fx2+fx3+fx4+fx5+fx6+fx7
fi=frek1+frek2+frek3+frek4+frek5+frek6+frek7
simpang.rata=fixi/fi
simpang.rata

#simpangan baku
fix1=frek1*((median1-rata)**2)
fix2=frek2*((median2-rata)**2)
fix3=frek3*((median3-rata)**2)
fix4=frek4*((median4-rata)**2)
fix5=frek5*((median5-rata)**2)
fix6=frek6*((median6-rata)**2)
fix7=frek7*((median7-rata)**2)
fixi2= fix1+fix2+fix3+fix4+fix5+fix6+fix7
simpang.baku = (fixi2/fi)**(1/2)
simpang.baku

# median(Nilai Tengah).
median.result <- median(data)
print(median.result)

# 5. Simpangan Rata rata
SR <- 506.498 / 213
print(SR)

# 6. Simpangan baku 
s <- sqrt(SR)
print(s)

