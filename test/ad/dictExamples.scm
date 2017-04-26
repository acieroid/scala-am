(define nl->fr (create-dictionary))
(nl->fr 'insert 'fiets '(bicyclette))
(nl->fr 'insert 'auto '(voiture))
(nl->fr 'insert 'huis '(maison))
(nl->fr 'insert 'vrachtwagen '(camion))
(nl->fr 'insert 'tientonner '(camion))
(nl->fr 'lookup 'fiets)(nl->fr 'display)

(define fr->eng (create-dictionary))
(fr->eng 'insert 'bicyclette '(bike))
(fr->eng 'insert 'voiture '(car))
(fr->eng 'insert 'maison '(house home))
(fr->eng 'insert 'camion '(truck))
(fr->eng 'lookup 'bicyclette)




