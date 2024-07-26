% PARCIAL RALLY DAKAR

%Nos entregan la siguiente base de conocimiento para poder trabajar, con los siguientes functores:
% - auto(modelo)
% - moto(anioDeFabricacion, suspensionesExtras)
% - camion(items)
% - cuatri(marca)
% PERO en un futuro podrian haber mas

%ganador(AnioEnQueGano, Ganador, VehiculoConQueGano)
ganador(1997,peterhansel,moto(1995, 1)).
ganador(1998,peterhansel,moto(1998, 1)).
ganador(2010,sainz,auto(touareg)).
ganador(2010,depress,moto(2009, 2)).
ganador(2010,karibov,camion([vodka, mate])).
ganador(2010,patronelli,cuatri(yamaha)).
ganador(2011,principeCatar,auto(touareg)).
ganador(2011,coma,moto(2011, 2)).
ganador(2011,chagin,camion([repuestos, mate])).
ganador(2011,patronelli,cuatri(yamaha)).
ganador(2012,peterhansel,auto(countryman)).
ganador(2012,depress,moto(2011, 2)).
ganador(2012,deRooy,camion([vodka, bebidas])).
ganador(2012,patronelli,cuatri(yamaha)).
ganador(2013,peterhansel,auto(countryman)).
ganador(2013,depress,moto(2011, 2)).
ganador(2013,nikolaev,camion([vodka, bebidas])).
ganador(2013,patronelli,cuatri(yamaha)).
ganador(2014,coma,auto(countryman)).
ganador(2014,coma,moto(2013, 3)).
ganador(2014,karibov,camion([tanqueExtra])).
ganador(2014,casale,cuatri(yamaha)).
ganador(2015,principeCatar,auto(countryman)).
ganador(2015,coma,moto(2013, 2)).
ganador(2015,mardeev,camion([])).
ganador(2015,sonic,cuatri(yamaha)).
ganador(2016,peterhansel,auto(2008)).
ganador(2016,prince,moto(2016, 2)).
ganador(2016,deRooy,camion([vodka, mascota])).
ganador(2016,patronelli,cuatri(yamaha)).
ganador(2017,peterhansel,auto(3008)).
ganador(2017,sunderland,moto(2016, 4)).
ganador(2017,nikolaev,camion([ruedaExtra])).
ganador(2017,karyakin,cuatri(yamaha)).
ganador(2018,sainz,auto(3008)).
ganador(2018,walkner,moto(2018, 3)).
ganador(2018,nicolaev,camion([vodka, cama])).
ganador(2018,casale,cuatri(yamaha)).
ganador(2019,principeCatar,auto(hilux)).
ganador(2019,prince,moto(2018, 2)).
ganador(2019,nikolaev,camion([cama, mascota])).
ganador(2019,cavigliasso,cuatri(yamaha)).

%pais(Conductor, PaisConductor)
pais(peterhansel,francia).
pais(sainz,espania).
pais(depress,francia).
pais(karibov,rusia).
pais(patronelli,argentina).
pais(principeCatar,catar).
pais(coma,espania).
pais(chagin,rusia).
pais(deRooy,holanda).
pais(nikolaev,rusia).
pais(casale,chile).
pais(mardeev,rusia).
pais(sonic,polonia).
pais(prince,australia).
pais(sunderland,reinoUnido).
pais(karyakin,rusia).
pais(walkner,austria).
pais(cavigliasso,argentina).

% 1) Agregar la siguiente información a la base de conocimientos:
% a) La marca peugeot tiene los modelos 2008 y 3008 de autos. 
% El countryman es modelo de auto marca mini, 
% touareg es marca volkswagen, y hilux es de marca toyota.

%marcaDelModelo(Marca, Modelo).
marcaDelModelo(peugeot, 2008).
marcaDelModelo(peugeot, 3008).
marcaDelModelo(mini, countryman).
marcaDelModelo(volkswagen, touareg).
marcaDelModelo(toyota, hilux).

% b) Teórico: ¿Qué debo agregar si quiero decir que el modelo 
% buggy es marca mini pero el modelo dkr no lo es? Justificar conceptualmente.

% Si quiero que el modelo buggy es marca mini, debo agregar a mi base de conocimientos:
marcaDelModelo(mini, buggy).
% Mientras tanto para el caso del modelo dkr que NO es marca mini entonces NO es necesario
% agregarlo debido al principio del Universo Cerrado de la base de conocimientos que me determina
% que todo lo que no este dentro de ella se considera falso, entonces al preguntar:
% -? marcaDelModelo(mini, dkr).
% false.

% 2) ganadorReincidente/1. Se cumple para aquel competidor que ganó en más de un año.

ganadorReincidente(Ganador) :- % -> Existen dos años en los que gano y esos años son diferentes
    ganoEn(UnAnio, Ganador),
    ganoEn(OtroAnio, Ganador),
    UnAnio \= OtroAnio.

ganoEn(Anio, Ganador) :-
    ganador(Anio, Ganador, _).

% 3) inspiraA/2. Un conductor resulta inspirador para otro cuando
% ganó y el otro no, y también resulta inspirador cuando ganó 
% algún año anterior al otro. En cualquier caso, el inspirador 
% debe ser del mismo país que el inspirado.

% el "y tambien" --> seria un "o" logico

%inspiraA(ConductorInspiracion, Condutor) :-
%    ganoEn(_, ConductorInspiracion),
%    not(ganoEn(_, Condutor)),
%    sonDelMismoPais(ConductorInspiracion, Conductor).

%inspiraA(ConductorInspiracion, Conductor) :-
%    ganoAntes(ConductorInspiracion, Conductor),
%    sonDelMismoPais(ConductorInspiracion, Conductor).

inspiraAV2(Inspirador, Inspirado) :-            % en este predicado, meti las dos opciones en puedeInspirar, asi no hay repeticion de logica como los de arriba
    puedeInspirar(Inspirador, Inspirado),
    sonDelMismoPais(Inspirador, Inspirado).

puedeInspirar(Inspirador, Conductor) :-  % puede inspirar si el inspirador gano y el inspirado NO
    ganoEn(_, Inspirador),
    not(ganoEn(_, Conductor)).

puedeInspirar(Inspirador, Conductor) :-  % puede inspirar si el ispirador gano antes que el inspirado
    ganoAntes(Inspirador, Conductor).

ganoAntes(Conductor1, Conductor2) :-
    ganoEn(Anio1, Conductor1),
    ganoEn(Anio2, Conductor2),
    Anio1 < Anio2.

sonDelMismoPais(Conductor1, Conductor2) :-
    pais(Conductor1,Pais),
    pais(Conductor2, Pais),
    Conductor1 \=  Conductor2.
%
% 4) marcaDeLaFortuna/2. Relaciona un conductor con una marca si sólo ganó con vehículos de esa marca. 
% Si un conductor nunca ganó, no debe tener marca de la fortuna.
% - La marca de un auto se obtiene a partir del modelo del auto. 
% - La marca de las motos dependen del año de fabricación: las fabricadas a partir del 2000 inclusive son ktm, las anteriores yamaha.
% - La marca de los camiones es kamaz si lleva vodka, sino la marca es iveco.
% - La marca del cuatri se indica en cada uno.

% si solo gano con vehiculos de esa marca --> NO gano con vehiculos de otra marca 

marcaDeLaFortuna(Conductor, Marca) :-
    ganoEn(_, Conductor),                   % el conductor tiene que haber ganado alguna vez
    usoMarca(Conductor, Marca),
    not(ganoConOtraMarca(Conductor,Marca)).

marcaDeLaFortunaFORALL(Conductor, Marca) :-
    ganoEn(_, Conductor),
    forall(ganador(_,Conductor,_), usoMarca(Conductor,Marca)).

ganoConOtraMarca(Conductor, Marca) :-
    usoMarca(Conductor, Marca),
    usoMarca(Conductor, OtraMarca),
    Marca \= OtraMarca.

usoMarca(Conductor, Marca) :-           % uso la marca para ganar
    ganador(_, Conductor, Vehiculo),
    marcaVehiculo(Vehiculo, Marca).     % asocio el vehiculo con su marca

marcaVehiculo(auto(Modelo), Marca) :-
    marcaDelModelo(Marca, Modelo).

marcaVehiculo(Moto, kmt) :-
    esMoto(Moto),
    fabricadaDesde(2000,Moto).

marcaVehiculo(Moto, yamaha) :-
    esMoto(Moto),
    not(fabricadaDesde(2000,Moto)).

marcaVehiculo(Camion, kamaz) :-
    esCamion(Camion),
    lleva(vodka, Camion).
    
marcaVehiculo(Camion, iveco) :-
    esCamion(Camion),
    not(lleva(vodka, Camion)).

marcaVehiculo(cuatri(Marca), Marca).

%marcaVehiculo(moto(AnioDeFabricacion, _), Marca) :-
%    marcaSegunAnio(AnioDeFabricacion, Marca).

%marcaSegunAnio((AnioDeFabricacion), ktm) :- AnioDeFabricacion >= 2000.   %
%marcaSegunAnio((AnioDeFabricacion), yamaha) :- AnioDeFabricacion < 2000. %

fabricadaDesde(AnioDesde, moto(AnioDeFabricacion,_)) :-
    AnioDeFabricacion >= AnioDesde.

lleva(Elemento, camion(Items)) :-
    member(Elemento, Items).

esCamion(camion(_)).
esMoto(moto(_,_)).

%marcaVehiculo(camion(Items), Marca) :-
%    marcaSegunItems(Items, Marca).

%marcaSegunItems(Items, kamaz) :-
%    member(vodka, Items).

%marcaSegunItems(Items, iveco) :-
%    not(member(vodka, Items)).

    
% 5) heroePopular/1. Decimos que un corredor es un héroe popular cuando sirvió de inspiración a alguien, y además el año que salió ganador 
% fue el único de los conductores ganadores que no usó un vehículo caro.
% Un vehículo es caro cuando es de una marca cara (por ahora las caras son mini, toyota e iveco), o tiene al menos tres suspensiones extras. 
% La cantidad de suspensiones extras que trae una moto se indica en cada una, los cuatri llevan siempre 4, y los otros vehículos ninguna.

/* heroePopular(Conductor) :- se cumple cuando
  - Sirvio de inspiracion a alguien
  - Fue el unico en NO usar un vehiculo caro el anio que gano  
*/

heroePopular(Conductor) :-
    inspiraAV2(Conductor,_),
    unicoSinVehiculoCaro(Conductor).

/* unicoSinVehiculoCaro(Conductor) :- se cumple cuando
  - No uso un vehiculo caro el año que gano
  - Para todo otro ganador de ese mismo año, este si utilizo un vehiculo caro 
*/

unicoSinVehiculoCaro(Conductor) :-
    ganoEn(Anio, Conductor),
    not(usoVehiculoCaro(Conductor, Anio)),
    forall(ganadorDiferente(Anio, Conductor, OtroConductor), usoVehiculoCaro(OtroConductor, Anio)). % para todo otro ganador de ese mismo año, este utlizo un vehiculo caro en ese año

ganadorDiferente(Anio, Conductor, OtroConductor) :-  % en el mimso, hubo dos ganadores diferentes
    ganoEn(Anio, Conductor),
    ganoEn(Anio, OtroConductor),
    Conductor \= OtroConductor.

%usoVehiculo(Conductor, Vehiculo) :- ganador(_,Conductor,Vehiculo).

usoVehiculoCaro(Conductor, Anio) :-             % ese conductor en tal año uso un vehiculo caro
    ganador(Anio, Conductor, Vehiculo),
    vehiculoCaro(Vehiculo).

vehiculoCaro(Vehiculo) :-
    marcaVehiculo(Vehiculo, Marca),
    marcaCara(Marca).

vehiculoCaro(Vehiculo) :- 
    cantidadSuspensiones(Vehiculo, SuspensionesExtras),
    SuspensionesExtras >= 3.

cantidadSuspensiones(moto(_, CantSuspensiones), CantSuspensiones).
cantidadSuspensiones(cuatri(_), 4).
%cantidadSuspensiones(auto(_), CantSuspensiones) :- CantSuspensiones is 0.
%cantidadSuspensiones(camion(_), CantSuspensiones) :- CantSuspensiones is 0. 

marcaCara(mini).
marcaCara(toyota).
marcaCara(iveco).

% ----------------------------------------------------------------------------------
% PUNTO 6

%etapa(Origen, Destino, Distancia)
etapa(marDelPlata,santaRosa,60).
etapa(santaRosa,sanRafael,290).
etapa(sanRafael,sanJuan,208).
etapa(sanJuan,chilecito,326).
etapa(chilecito,fiambala,177).
etapa(fiambala,copiapo,274).
etapa(copiapo,antofagasta,477).
etapa(antofagasta,iquique,557).
etapa(iquique,arica,377).
etapa(arica,arequipa,478).
etapa(arequipa,nazca,246).
etapa(nazca,pisco,276).
etapa(pisco,lima,29).

% A) Necesitamos un predicado que permita saber cuántos kilómetros existen entre dos locaciones distintas. 
% ¡Atención! Debe poder calcularse también entre locaciones que NO pertenezcan a la misma etapa. 
% Por ejemplo, entre sanRafael y copiapo hay 208+326+177+274 = 985 km

cuantosKilometros(Origen, Destino, Distancia) :-            % CASO BASE: si la distacia es directa (pertenecen a la misma etapa)
    etapa(Origen, Destino, Distancia).

cuantosKilometros(Origen, Destino, DistaciaTotal) :-        % CASO RECURSIVO: si la distancia es indirecta (NO pertenecen a la misma etapa)
    etapa(Origen, PuntoIntermedio, DistanciaIntermedia),            
    cuantosKilometros(PuntoIntermedio, Destino, DistaciaRestante),
    DistaciaTotal is DistanciaIntermedia + DistaciaRestante.

% B) Saber si un vehículo puede recorrer cierta distancia sin parar. Por ahora (posiblemente cambie) diremos que un vehículo caro puede recorrer 2000 km, 
% mientras que el resto solamente 1800 km. Además, los camiones pueden también recorrer una distancia máxima igual a la cantidad de cosas que lleva * 1000.
% Por ejemplo, una moto(1999,1) como no es cara, puede recorrer 1800 km pero no 1900 km.

puedeRecorrerSinParar(Vehiculo, Distancia) :-  % puede recorrer cualquier distancia, siempre menor a su limite!!
    limiteVehiculo(Vehiculo, Limite),
    Distancia =< Limite.

limiteVehiculo(Vehiculo, 2000) :- vehiculoCaro(Vehiculo).
limiteVehiculo(Vehiculo, 1800) :- not(vehiculoCaro(Vehiculo)).

limiteVehiculo(camion(Items), Limite) :- 
    length(Items, CantItems),
    Limite is CantItems * 1000.

% C) Los corredores quieren saber, dado un vehículo y un origen, cuál es el destino más lejano al que pueden llegar sin parar.
% Para la moto del punto anterior el destino más lejano desde marDelPlata es copiapo. Ya suman 1335 km, pero el con el próximo destino (antofagasta) 
% se va a 1812 km, que es una distancia que no puede recorrer.

%destinosQuePuedeRealizar(Vehiculo, Origen, Destino, DistaciaTotal) :-
%    puedeRecorrerSinParar(Vehiculo, DistanciaMaxima),
%    cuantosKilometros(Origen, Destino, DistaciaTotal),
%    DistanciaMaxima >= DistaciaTotal.

%distanciaMasLejana(Vehiculo, Origen, DistanciaMasLejana) :-
%    findall(Distancias, destinosQuePuedeRealizar(Vehiculo, Origen, _ , Distancias), ListaDeDistacias),
%    max_member(DistanciaMasLejana, ListaDeDistacias).
    
destinoMasLejano(Vehiculo, Origen, DestinoMasLejano) :-
    puedeLlegarSinParar(Vehiculo, Origen, DestinoMasLejano),  % primero que pueda llegar al destino mas lejano
    forall(otroDestinoPosible(Vehiculo, Origen, DestinoMasLejano, OtroDestino), estaMasCerca(OtroDestino, Origen, DestinoMasLejano)). % para todos aquellos otros destinos posibles, dichos destinos son mas cercarnos al destino mas lejano
                                                                                                                                      % es decir para todo otro destino posible, está más cerca del origen (distancia menor desde origen)
puedeLlegarSinParar(Vehiculo, Origen, Destino) :-    % puede llegar sin parar si puede recorrer la distancia que hay entre el origen y el destino
    cuantosKilometros(Origen, Destino, Distancia),
    puedeRecorrerSinParar(Vehiculo, Distancia).

estaMasCerca(DestinoCercano, Origen, Destino) :-
    cuantosKilometros(Origen, Destino, Distacia),
    cuantosKilometros(Origen, DestinoCercano, DistanciaCercana),
    DistanciaCercana < Distacia.

otroDestinoPosible(Vehiculo, Origen, Destino, OtroDestino) :-
    puedeLlegarSinParar(Vehiculo, Origen, Destino),
    puedeLlegarSinParar(Vehiculo, Origen, OtroDestino),
    Destino \= OtroDestino.
