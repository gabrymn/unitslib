# Struttura algebrica delle unità di misura
<br>

L’insieme delle unità di misura è un gruppo abeliano moltiplicativo.

Sia $U$ l’insieme di tutte le unità (base e derivate), la struttura algebrica è $(U, \cdot)$

Valgono quindi le proprietà di chiusura, associatività, elemento neutro, inverso, commutatività


### Chiusura
Per ogni $u_1, u_2 \in U$, vale $u_1 \cdot u_2 \in U$.

### Associatività
Per ogni $u_1, u_2, u_3 \in U$, vale $(u_1 \cdot u_2) \cdot u_3 = u_1 \cdot (u_2 \cdot u_3)$.

### Elemento neutro
Esiste l’unità adimensionale $1 \in U$ tale che $u \cdot 1 = 1 \cdot u = u$ per ogni $u \in U$.

### Inverso
Per ogni $u \in U$ esiste $u^{-1} \in U$ tale che $u \cdot u^{-1} = 1$.

### Commutatività
Per ogni $u_1, u_2 \in U$, vale $u_1 \cdot u_2 = u_2 \cdot u_1$.

<br>
<br>

Nel Sistema Internazionale le unità fondamentali sono:

- metro ($m$)
- chilogrammo ($kg$)
- secondo ($s$)
- ampere ($A$)
- kelvin ($K$)
- mole ($mol$)
- candela ($cd$)

<br>
<br>

Ogni unità derivata o base $u \in U$ può essere scritta come $u = m^{k_1} kg^{k_2} s^{k_3} A^{k_4} K^{k_5} mol^{k_6} cd^{k_7}$ con $k_i \in \mathbb{Z}$.  

Moltiplicare due unità corrisponde a sommare i vettori esponenti associati: $(k_1,\dots,k_7) + (h_1,\dots,h_7) = (k_1+h_1,\dots,k_7+h_7)$.

L'inversione di un'unità corrisponde al cambio di segno del vettore esponenti: $(k_1,\dots,k_7) \to (-k_1,\dots,-k_7)$.

L’unità adimensionale corrisponde al vettore nullo: $(0,0,0,0,0,0,0)$.

Osservazione: $U \cong \mathbb{Z}^7$


