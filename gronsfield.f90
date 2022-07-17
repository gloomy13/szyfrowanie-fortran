	integer :: i
	integer :: iklucza
	integer :: dlklucza
	integer :: przesuniecie
	character(9) :: polecenie
	character(10) :: kluczc
	integer :: klucz
	character(100) :: tekst
	character,dimension(100) :: rezultat

1   print *,'Wprowadz dane (tylko DUZE litery) w kolejnosci: polecenie -> klucz -> tekst'
    read *,polecenie,kluczc,tekst

    read(kluczc,*) klucz  !zamiana char na integer

	!rzeczywisty rozmiar tablicy (bez pustych elementow)
    dlklucza=0
    i=1
    do while(kluczc(i:i) /= '')
    	dlklucza=dlklucza+1
    	i=i+1
    end do

    !czyszczenie tablicy
    do i=1,100
    	rezultat(i:i)=''
    end do

    i=1
    iklucza=dlklucza

    if (polecenie == 'SZYFRUJ') then
    	print *,'szyfrowanie...'
    	do while (tekst(i:i) /= '')
    		przesuniecie = klucz/(10**(iklucza-1)) - (klucz/10**(iklucza)*10)

    		if(iachar(tekst(i:i)) + przesuniecie > 90) then		! sprawdzenie czy wykracza poza Z
    			rezultat(i:i)=achar(iachar(tekst(i:i)) + przesuniecie - 26) ! 26 to odleglosc A-Z
    		else
    			rezultat(i:i)=achar(iachar(tekst(i:i)) + przesuniecie)
    		endif
    		i=i+1
    		if(iklucza == 1) then
    			iklucza=dlklucza
    		else
				iklucza=iklucza-1
			end if
    	end do
    else if (polecenie == 'DESZYFRUJ') then
    	print *,'deszyfrowanie...'
    	do while (tekst(i:i) /= '')
    		przesuniecie = klucz/(10**(iklucza-1)) - (klucz/10**(iklucza)*10)

    		if(iachar(tekst(i:i)) - przesuniecie < 65) then
    			rezultat(i:i)=achar(iachar(tekst(i:i)) - przesuniecie + 26) ! 26 to odleglosc A-Z
    		else
    			rezultat(i:i)=achar(iachar(tekst(i:i)) - przesuniecie)
    		endif
    		i=i+1
    		if(iklucza == 1) then
    			iklucza=dlklucza
    		else
				iklucza=iklucza-1
			end if
    	end do
    else
    	print *,'Zle polecenie'
    	go to 1
    end if

    print *,rezultat

  	end