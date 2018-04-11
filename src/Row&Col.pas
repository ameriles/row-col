{****************************************************************************
                            Row&Col VERSION 1.00
                      DESARROLLADO POR: AGUSTIN MERILES
****************************************************************************}
program RowAndCol;
uses
    crt, dos, ugo, keyconst;
const
     TiempoTecla = 8;     {Tiempo que debera transcurrir para la repeticion}
     TiempoFicha = 50;    {Tiempo que debera transcurrir para que la ficha avance}
     TiempoGral  = 30;    {Tiempo que se puede usar para varias cosas}
     LimDcho     = 210;   {Area de Juego en pantalla}
     LimIzq      = 110;
     LimInf      = 180;
     Maxi        = 16;    {Tama¤o de la matriz de juego}
     Maxj        = 10;
     Copa        = 32;    {Numero de frame en el archivo bsf para el objeto copa}
     Lata        = 35;    {''     ''  ''              ''                ''  lata}
     Monitor     = 37;    {''    ''           ''               ''           monitor}
type
    {Estructura de datos Ficha}
    columna = array [1..4] of byte;
    Ficha = record
      x, y   : word;       {coordenadas de la ficha}
      col    : columna;    {arreglo que guarda la ficha}
      colsig : columna;    {arreglo que guarda la ficha sig}
      time   : byte;       {guarda el tiempo que necesita para avanzar}
      vidas  : byte;       {guarda la cantidad de vidas}
      puntos : longint;    {puntaje acumulado}
      combo  : byte;       {Contador para los combos}
      bombas : byte;       {Contador de bombas}
      laser  : byte;       {Contador de laser}
      desv   : byte;       {Contador de desvanecedores}
      pow    : byte;       {Identificador de que tipo de poder es}
      viva   : boolean     {indica si la ficha esta apta para ser movida}
    end;

    {tipo de datos OBJETO}
    objeto = record
      col   : byte;            {numero de olumna que ocupa el objeto en la matriz}
      tipo  : byte;            {tipo de objeto que es (copa, lata, monitor)}
      vivo  : boolean;         {indica si todavia esta vivo}
      danio : byte             {indica el da¤o que se le a ocasionado al objeto}
    end;

    {tpo de datos array que contendra los tres objetos}
    Objetos = record
      arr : array[1..3] of objeto;      {arreglo propiamente dicho}
      num : byte                        {numero de objetos vivos que contiene el array}
    end;

    MatrizJuego = array [1..16, 1..10] of byte; {Tipo de datos Matriz de Juego}

    TNivel = record
      matriz : array[1..10, 1..10] of byte; {Tipo de datos que almacenara el nivelen el archivo}
    end;

    Coord=record   {tipo de datos coordenada}
      x, y : word
    end;

    PilaCoord=array[1..10] of Coord;  {tipo de datos pila de coordenadas}

    Hi = record                       {tipo de datos Hiscore}
      nombre : string[3];             {nombre, compuesto de 3 letras}
      puntos : longint                {puntos que se obuvieron}
    end;
var
   fondo      : byte;       {guarda la el color de fondo}
   n          : byte;       {guarda la cantidad de sprites que contiene Sprites}
   Sprites    : SpriteBSF;  {Guarda los sprites del juego}
   Pal        : Paleta;     {guarda la paleta del juego}
   Pant       : PV;         {Pantalla Virtual Pricipal del juego}
   HTecla     : Boolean;    {Evitara la presion "turbo" de una tecla}
   UltTecla   : Byte;       {Guarda el tiempo transcurrido desde la presion de la ultima tecla}
   P1         : Ficha;      {Jugador 1}
   AreaJ1     : MatrizJuego;{Area de Juego del Jugador 1}
   Objs1      : Objetos;    {Objs1 contendra los objetos del area 1}
   Nivel      : Byte;       {Contador que lleva el numero de nivel}
   hay_nivel  : boolean;    {bandera que indica si hay niveles disponibles en el archivo}
   GameOver   : Boolean;    {indica si se termino el juego (mas que obvio)}
   Fin        : Boolean;    {se pone en verdadero si se termino el nivel}
   PilaC      : PilaCoord;  {Pila que contiene las coordenadas de los efectos de los numeritos}
   TopeC      : byte;       {Tope de esta pila}
   combo      : byte;       {contador auxiliar de combos}
   combotime  : byte;       {contiene el tiempo que se mantendra la frase X COMBO en la pantalla, no es de gran imprtancia}
   TiempoAnim : byte;       {contiene el tiempo que durara la animacion de la explosion}
   Frame      : byte;       {variable usada para direccionar varios frames dentro del archivo BSF}
   powx, powy : word;       {coordenadas de las animaciones de los poderes}
   NumObjs    : byte;       {contiene el numero de obetos que hay vivos actualmente en el juego}
   LimYnum10  : word;       {variable que contendra el limite hasta el cual podran subir los numeritos 10}
   framellama : byte;       {variable para direccionar el frame de las llamas en el archivo BSF}
   tllama     : byte;       {tiempo que se mantendra el frame de la llama en pantalla}
   Timer      : byte;       {Reloj Timer}
   SegAnt     : word;       {usada para mantener los segundos anteriores para que el reloj descienda correctamente}

   t_acelera  : byte;   {Variables para que el usuario defina sus teclas}
   t_shoot    : byte;
   t_desv     : byte;
   t_bomba    : byte;
   t_laser    : byte;
   t_pausa    : byte;
   t_up       : byte;
   t_down     : byte;
   t_left     : byte;
   t_right    : byte;

   toc        : sample; {sample del ruidito de la colision}
   sound      : sample; {sample para todos los demas sonidos}

   HiRecord   : file of Hi;  {Archivo que contendra los records}
   HiScore    : Hi;          {Registro para leer del archivo de Records}

   salir      : boolean;     {variable bandera para salir del juego}

{Espera el retrazado vertical del monitor}
procedure Retrace;
begin
     repeat
     until (port[$3da] and 8)=8;
     repeat
     until (port[$3da] and 8)<>8;
end;

{Dibuja todo lo que hay en la pantalla virtual a la memoria de video}
procedure Actualizar;
begin
     Retrace;   {Espera el retrazado}
     CopiaPV(Pant,Pantalla,0,0,0,0,320,200)
end;

procedure Inicializaciones;          {Inicia Todas las variables del juego}
begin
     IniciaUgo;
     IniciaModoGrafico(VGA, 320, 200);
     InstalaTeclado;
     DetectaSB;
     CargaPaleta('Fichas.pal', Pal);
     CambiaPaleta(Pal);
     CreaPV(320, 200, Pant);
     CargaBSF('Fichas.bsf', Sprites, n);
     ActivaClaveBTF('samples.btf','');
     BTFCargaSample('samples.btf', 'toc.pcm', toc);

     t_up:=t_arriba;        {teclas por defecto}
     t_down:=t_abajo;
     t_left:=t_izq;
     t_right:=t_dcha;
     t_pausa:=t_p;
     t_bomba:=t_b;
     t_desv:=t_d;
     t_laser:=t_l;
     t_shoot:=t_enter;
     t_acelera:=t_espacio
end;

{Inicializa todo lo necesario para comenzar una Nueva Partida}
Procedure NuevoJuego;
begin
     fondo:=28;
     GameOver:=false;
     Fin:=false;
     ComboTime:=255;
     P1.bombas:=0;
     P1.desv:=0;
     P1.laser:=0;
     P1.vidas:=2;
     P1.Puntos:=0;
     P1.pow:=0;
     TiempoAnim:=0;
     Nivel:=1;
     framellama:=42;
     Tllama:=0;
     Timer:=120;
     SegAnt:=65535;
     assign(HiRecord, 'records.dat');
     {$I-}
     reset(HiRecord);
     {$I-}
     if (ioresult<>0) then rewrite(HiRecord);
     close(HiRecord);

     reset(HiRecord);
     read(HiRecord, HiScore);
     close(HiRecord)
end;

{Calcula las posiciones i,j en la matriz a partir de la coordenadas x e y}
procedure XYaIJ(x, y : word; var i, j : byte);
begin
     i:=(y-10) div 10;
     j:=(x-100) div 10
end;

{Calcula las posiciones x y en la pantalla partir de los indices i, j}
procedure IJaXY(i, j : word; var x, y : word);
var
   x2, y2 : word;
begin
     asm
       mov ax, j        {Estos calculos equivalen a x:=(j*10)+100}
       mov bx, j                                {   y:=(i*10)+10 }
       shl ax, 1        { pero son teoricamente mas rapidos      }
       shl bx, 3
       add ax, bx
       add ax, 100
       mov x2, ax

       mov ax, i
       mov bx, i
       shl ax, 1
       shl bx, 3
       add ax, bx
       add ax, 10
       mov y2, ax
    end;
    x:=x2;
    y:=y2
end;

{Devuelve el numero de elementos que hay en la ficha col}
function NumElemColumna(col : columna) : byte;
var
   n, i : byte;
begin
     n:=0;
     for i:=1 to 4 do
       if col[i]<>0
         then
           inc(n,1);
     NumElemColumna:=n
end;

{Desplaza los elementos de la ficha hacia arriba}
procedure RotarArriba(var Col : Columna);
var
   i, n : byte;
   aux  : byte;
begin
     n:=NumElemColumna(col);
     aux:=col[1];
     for i:=1 to (n-1) do
       col[i]:=col[i+1];
     col[n]:=aux
end;

{Desplaza los elementos de la ficha hacia abajo}
procedure RotarAbajo(var Col : Columna);
var
   i, n : byte;
   aux  : byte;
begin
     n:=NumElemColumna(col);
     aux:=col[n];
     for i:=n downto 2 do
       col[i]:=col[i-1];
     col[1]:=aux
end;

{Crea los objetos que se deben hacer caer hasta abajo}
function CreaObjetos(var Objs : Objetos; i, col : byte) : byte;
var
   ob, x : byte;
begin
     randomize;
     x:=random(3);
     case x of
       0 : ob:=copa;
       1 : ob:=lata;
       2 : ob:=monitor
     end;

     objs.num:=i;
     objs.arr[i].col:=col;
     objs.arr[i].tipo:=ob;
     objs.arr[i].vivo:=true;
     objs.arr[i].danio:=0;

     NumObjs:=objs.num;
     CreaObjetos:=ob
end;

{Crea el area de Juego}
procedure CreaAreaJuego(var area : MatrizJuego);
var
   i, j, c : byte;
   Niveles : file of TNivel;  {Archivo que contiene los niveles}
   areaniv : TNivel;
begin
     for i:=1 to maxi do          {limpia la matriz}
       for j:=1 to maxj do
         area[i,j]:=0;

     assign(Niveles, 'Niveles.dat');
     reset(Niveles);
     if (filesize(Niveles) < nivel)
       then
         begin
           hay_nivel:=false;
           gameover:=true;
           close(Niveles);
         end
       else
         begin
           hay_nivel:=true;
           seek(Niveles, nivel-1);
           read(Niveles, areaniv);

           c:=0;
           for i:=10 downto 1 do          {copia el nivel}
             for j:=1 to maxj do
               begin
                 if (areaniv.matriz[i,j]=9)
                   then
                     begin
                       inc(c, 1);
                       area[i+6, j]:=CreaObjetos(Objs1, c, j)
                     end
                   else
                     area[i+6,j]:=areaniv.matriz[i,j];
               end;
           close(Niveles)
         end
end;

{Devuelve verdadero si la fila i esta vacia en la matriz}
function LineaVacia(i : byte; area : MatrizJuego) : boolean;
var
   vacia : boolean;
   j     : byte;
begin
     j:=1;
     vacia:=true;
     while (j<=maxj) and vacia do
       begin
         if area[i,j]<>0
           then
             vacia:=false;
         inc(j, 1)
       end;
     LineaVacia:=vacia
end;

{Devuelve verdadero si los 3 objetos ya llegaron al piso}
function LlegaronObjetos(Objs : Objetos; area : MatrizJuego) : boolean;
var
   i, j, c : byte;
   lleg    : boolean;
begin
     lleg:=false;
     c:=0;
     for j:=1 to maxj do
       if (area[maxi, j]>=copa) and (area[maxi, j]<=monitor+1)
         then
           inc(c, 1);

     if c=Objs.Num then lleg:=true;
     if Objs.Num=0 then dec(Nivel, 1);

     LlegaronObjetos:=lleg
end;

{Se ejecuta si el objeto ha sido da¤ado por la explosion}
procedure ModEstadoObjeto(var Obj : Objeto);
begin
     inc(Obj.danio, 1);
     case Obj.tipo of
       copa    : if Obj.danio=3 then Obj.vivo:=false;
       lata    : if Obj.danio=2 then Obj.vivo:=false;
       monitor : if Obj.danio=2 then Obj.vivo:=false
     end
end;

{Devuelve la fila en la que se encuentra el objeto}
function IndObjeto(j : byte; Objs : Objetos) : byte;
var
   ind, i : byte;
begin
     ind:=0;
     for i:=1 to 3 do
       if (j=Objs.arr[i].col)
         then
           ind:=i;
     IndObjeto:=ind
end;

{devuelve un elemento que compondra la ficha}
function elemento : byte;
var
   i, j, e : byte;
   listo   : boolean;
begin
     listo:=false;
     repeat
       e:=random(6)+1;
       i:=1;
       while (i<=maxi) and not listo do
         begin
           j:=1;
           while (j<=maxj) and not listo do
             begin
               if (AreaJ1[i, j] = e) then listo:=true;
               inc(j, 1);
             end;
           inc(i, 1)
         end;
     until listo;
     elemento:=e
end;

{Crea una ficha al azar y la asigna al jugador}
procedure CreaFicha(Var Player : Ficha);
var
   i, j, tipo : byte;
begin
     Player.x:=150;             {Coordenadas iniciales}
     Player.y:=20;
     Player.Viva:=True;         {La ficha se puede mover}
     Player.Time:=0;            {Pone el timer para avanzar en cero}
     Player.Combo:=0;           {Cero combos}
     Player.Pow:=0;             {No esta activado ningun poder}

     for i:=1 to 4 do
       Player.Col[i]:=Player.ColSig[i]; {Pasa la ficha NEXT a la actual}

     for i:=1 to 4 do                   {Limpia el vector que contendra la ficha}
       Player.ColSig[i]:=0;

    randomize;
    tipo:=random(4);

    case tipo of
    {Algoritmo que crea la ficha  X X X X}
    0 : for i:=1 to 4 do
          Player.ColSig[i]:=elemento;

    {Algoritmo que crea la ficha  X X X}
    1 : for i:=1 to 3 do
          Player.ColSig[i]:=elemento;

    {Algoritmo que crea la ficha  X X}
    2 : for i:=1 to 2 do
          Player.ColSig[i]:=elemento;

    {Algoritmo que crea la ficha  X}
    3 : Player.ColSig[1]:=elemento
    end
end;

{crea una Bomba}
procedure CreaBomba(var Player : Ficha);
var
   i       : byte;
begin
     PausaSMP(0);
     EliminaSample(sound);
     if BTFCargaSample('samples.btf', 'gunload.pcm', sound) = 0 then ReproduceSMP(sound, 11025);

     dec(player.bombas, 1);
     player.pow:=1;
     Player.Col[1]:=26;         {Creamos la ficha bomba}
     for i:=2 to 4 do
       player.col[i]:=0         {por lo tanto borramos los otros elementos}
end;

{Produce la explosion de la bomba}
procedure ExplosionBomba(player : ficha; var area : MatrizJuego; var Objs : Objetos);
var
   i, j, poder, ob    : byte;
   arr, izq, aba, der : integer;
begin
     XYaIJ(player.x, player.y, i, j);
     poder:=2;

     arr:=i-poder;             {calculo las coordenadas de la explosion}
     if arr<0 then arr:=0;
     izq:=j-poder;
     if izq<0 then izq:=0;
     aba:=i+poder;
     if aba>16 then aba:=16;
     der:=j+poder;
     if der>10 then der:=10;

     ob:=0;
     for i:=arr to aba do     {elimino los elementos afectados por la explosion}
       for j:=izq to der do
         if (area[i,j]>=copa) and (area[i,j]<=monitor+1)
           then
             begin
               ob:=IndObjeto(j,Objs);
               ModEstadoObjeto(Objs.arr[ob]);
               if Objs.arr[ob].vivo
                 then
                   inc(area[i,j], 1)
                 else
                   begin
                     dec(Objs.num, 1);
                     area[i,j]:=0
                   end
             end
           else
             area[i,j]:=0;
     i:=arr;
     j:=izq;
     IJaXY(i, j, powx, powy);
     Frame:=26;
     TiempoAnim:=25;

     PausaSMP(0);
     EliminaSample(sound);
     if BTFCargaSample('samples.btf', 'boom.pcm', sound) = 0 then ReproduceSMP(sound, 11025)
end;

{Dibuja la matriz en la pantalla}
procedure DibujaMatriz(area : MatrizJuego);
var
   i, j : byte;
   x, y : word;
begin
     for i:=1 to 16 do           {Dibuja la matriz del area de juego}
       for j:=1 to 10 do
         if Area[i,j]<>0
           then
             begin
               IJaXY(i, j, x, y);{Calcula las coordenadas x e y de acuerdo a su posicion en la matriz}
               PonSpriteBSF(x, y, Area[i,j], Sprites, Pant);
             end
end;

{Dibuja todos los elementos de la pantalla}
procedure DibujaPantalla(player : Ficha; area : MatrizJuego);
var
   i, j, h, m, s, ms : word;
   puntos            : string[10];
begin
     {Lineas delimitadoras}
     for i:=0 to 199 do
       PonPixel(LimIzq-1, i, 9, Pant);
     for i:=0 to 199 do
       PonPixel(LimDcho, i, 9, Pant);
     for i:=LimIzq to LimDcho do
       PonPixel(i, LimInf, 9, Pant);
     for i:=LimIzq to LimDcho do
       PonPixel(i, 19, 9, Pant);

     DibujaMatriz(area);

     {Escribe los datos del jugador y del juego}
     EscribeXY(Fnt, 235,100, 'Lives', 0, TXT_Normal, Pant);
     EscribeXY(Fnt, 250, 112, itos(Player.vidas), 22, TXT_Normal, Pant);

     EscribeXY(Fnt, 235, 130, 'Level', 0, TXT_Normal, Pant);
     EscribeXY(Fnt, 250, 142, itos(Nivel), 22, TXT_Normal, Pant);

     EscribeXY(Fnt, 212, 23, 'Time', 0, TXT_Ampliado, Pant);
     EscribeXY(Fnt, 220, 50, itos(Timer), 22, TXT_Bombilla, Pant);

     {Actualiza el timer}
     gettime(h,m,s,ms);
     if (SegAnt <> s) and (Player.viva)
       then
         dec(timer, 1);
     SegAnt:=s;

     {Animacion de las llamas}
     if (framellama < 45)
       then
         begin
           if (Tllama = 4)
             then
               begin
                 Tllama:=0;
                 inc(framellama, 1)
               end
             else
               inc(Tllama, 1)
         end
       else
         framellama:=42;

     {Dibuja las llamas}
     PonSpriteBSF(LimIzq, LimInf+1, framellama, Sprites, Pant);
     PonSpriteBSF(LimIzq+24, LimInf+1, framellama, Sprites, Pant);
     PonSpriteBSF(LimIzq+49, LimInf+1, framellama, Sprites, Pant);
     PonSpriteBSF(LimIzq+73, LimInf+1, framellama, Sprites, Pant);

     if player.combo>1
       then
         begin
           combo:=player.combo;  {para que no desaparezca apenas se actualiza la ficha}
           combotime:=0
         end;

     {escribe el X Combo hasta que pase un tiempo determinado}
     if ComboTime<(TiempoGral)
       then
         begin
           EscribeXY(Fnt, 15, 50, itos(combo)+'X COMBO', 24, TXT_Normal, Pant);
           inc(combotime, 1)
         end;

     {Dibuja la bomba y su contador al lado}
     PonSpriteBSF(33, 139, 26, Sprites, Pant);
     EscribeXY(Fnt, 42, 141, 'X '+itos(Player.bombas), 6, TXT_Normal, Pant);

     {Dibuja el laser y su contador al lado}
     PonSpriteBSF(33, 151, 40, Sprites, Pant);
     EscribeXY(Fnt, 42, 153, 'X '+itos(Player.laser), 22, TXT_Normal, Pant);

     {Dibuja el desvanecedor y su contador al lado}
     PonSpriteBSF(33, 163, 39, Sprites, Pant);
     EscribeXY(Fnt, 42, 165, 'X '+itos(Player.desv), 5, TXT_Normal, Pant);

     {Animacion de la explosion}
     if TiempoAnim>0                    {Cuando el Tiempo de la animacion llegue a 0 se acaba}
       then
         begin
           dec(TiempoAnim, 1);          {decremento en cada paso}
           if (TiempoAnim mod 5)=0      {si el resto del tiempo divido por 7 da cero, como incializa en (7x5)+1(nø de frames)}
             then                       {va a mostrar solo los 5 frames de la animacion. El 7 varia la velocidad}
               inc(Frame, 1);
           if Frame>26 then PonSpriteBSF(powx, powy, Frame, Sprites, Pant)       {Copiamos el sprite a la PV}
         end;

     {hace subir los numeritos 10}
     if (pilac[1].y > LimYnum10)         {si la coordenada y de los puntos es menor o igual a 75 desaparecen}
       then
         for i:=1 to topeC do   {desapila las coordenadas y pone los sprites}
           begin
             PonSpriteBSF(pilac[i].x, pilac[i].y, 25, Sprites, Pant);
             dec(pilac[i].y, 1)      {decrementa la coordenada Y para que suban}
           end;

     {Dibuja las armas si estas fueron seleccionadas}
     if player.pow=2 then PonSpriteBSF(player.x, player.y, 39, Sprites, Pant);
     if player.pow=3 then PonSpriteBSF(player.x, player.y, 40, Sprites, Pant);

     {Ficha Proxima}
     EscribeXY(Fnt, 30, 70, 'Next', 0, TXT_Normal, Pant);
     j:=85;
     for i:=1 to 4 do
       if Player.ColSig[i]<>0
         then
           begin
             PonSpriteBSF(49, j, Player.ColSig[i], Sprites, Pant);
             inc(j, 10)
           end;

     {Escribe los Records}
     str(HiScore.puntos, puntos);
     EscribeXY(Fnt, 110, 5, HiScore.nombre, 126, TXT_Normal, Pant);
     EscribeXY(Fnt, 150, 5, puntos, 126, TXT_Normal, Pant);
     str(Player.puntos, puntos);
     EscribeXY(Fnt, 26, 22, 'Score', 0, TXT_Normal, Pant);
     EscribeXY(Fnt, 33, 32, Puntos, 0, TXT_Normal, Pant)
end;

{Crea el Desvanecedor}
procedure CreaDesvanecedor(var player : Ficha);
var
   i       : byte;
begin
     PausaSMP(0);
     EliminaSample(sound);
     if BTFCargaSample('samples.btf', 'gunload.pcm', sound) = 0 then ReproduceSMP(sound, 11025);

     dec(player.desv, 1);
     player.pow:=2;
     Player.Col[1]:=39;         {Creamos la ficha desvanecedor}
     for i:=2 to 4 do
       player.col[i]:=0         {por lo tanto borramos los otros elementos}
end;

{Desvanece los elementos alcanzados por el desvanecedor}
procedure DesvanecerElementos(player : Ficha; var area : MatrizJuego;  var Objs : Objetos);
var
   elem, i, j, c, ob : byte;
begin
     PausaSMP(0);
     EliminaSample(sound);
     if BTFCargaSample('samples.btf', 'desv.pcm', sound) = 0 then ReproduceSMP(sound, 11025);

     XYaIJ(player.x, player.y+11, i, j);
     elem:=area[i,j];
     if (elem>=copa) and (elem<=monitor+1)
       then
         begin
           i:=maxi;
           while (i>=1) and not LineaVacia(i, area) do
             begin
               for j:=1 to maxj do
                 if area[i,j]=elem
                   then
                     begin
                       ob:=IndObjeto(j,Objs);
                       ModEstadoObjeto(Objs.arr[ob]);
                       if Objs.arr[ob].vivo
                         then
                           inc(area[i,j], 1)
                         else
                           begin
                             dec(Objs.num, 1);
                             area[i,j]:=0
                           end;
                       BorraPV(Pant, fondo);
                       DibujaPantalla(player, area);
                       Actualizar;
                     end;
               dec(i, 1);
             end
         end;

      if (elem<7)
        then
          for c:=0 to 3 do
            begin
              i:=maxi;
              while (i>=1) and not LineaVacia(i, area) do
                begin
                  for j:=1 to maxj do
                    if area[i,j]=elem+(c*6)
                      then
                        if c<>3
                          then
                            inc(area[i,j], 6)
                          else
                            area[i,j]:=0;
                  dec(i, 1);
                  BorraPV(Pant, fondo);
                  DibujaPantalla(player, area);
                  Actualizar
                end;
            end
end;

{Crea el Laser}
procedure CreaLaser(var player: Ficha);
var
   i       : byte;
begin
     PausaSMP(0);
     EliminaSample(sound);
     if BTFCargaSample('samples.btf', 'gunload.pcm', sound) = 0 then ReproduceSMP(sound, 11025);

     dec(player.laser, 1);
     player.pow:=3;
     Player.Col[1]:=40;         {Creamos la ficha desvanecedor}
     player.x:=LimIzq-16;
     for i:=2 to 4 do
       player.col[i]:=0         {por lo tanto borramos los otros elementos}
end;

{Dispara el laser y Hace desaparecer los elementos alcanzados por el laser}
procedure DisparoLaser(var player : Ficha; var area : MatrizJuego; var Objs : Objetos);
var
   i, j, ob  : byte;
   x         : word;
begin
     PausaSMP(0);
     EliminaSample(sound);
     if BTFCargaSample('samples.btf', 'laser.pcm', sound) = 0 then ReproduceSMP(sound, 11025);

     if player.y>=LimInf then player.y:=LimInf-10;

     x:=LimIzq;
     repeat
       BorraPV(Pant, fondo);
       DibujaPantalla(player, area);
       RectanguloSolido(LimIzq, player.y+2, x, player.y+8, 4,  Pant);
       Actualizar;
       inc(x, 5);
     until (x>=LimDcho);

     x:=LimIzq;
     repeat
       BorraPV(Pant, fondo);
       DibujaPantalla(player, area);
       RectanguloSolido(LimIzq, player.y+2, LimDcho, player.y+8, 4,  Pant);
       Actualizar;
       inc(x, 5);
     until (x>=LimDcho);

     XYaIJ(LimIzq+1, player.y, i, j);
     for j:=1 to maxj do
       begin
         if (area[i,j]>=copa) and (area[i,j]<=monitor+1)
           then
             begin
               dec(Objs.num, 1);
               ob:=IndObjeto(j, Objs);
               Objs.arr[ob].vivo:=false
             end;
         area[i,j]:=0
       end;
     Player.Viva:=false
end;

{Devuelve Verdadero si existe una ficha a la derecha de la ficha que se mueve}
function HayFDcha(Player : Ficha; area : MatrizJuego) : boolean;
var
   hay        : boolean;
   k, n, i, j : byte;
   x, y       : word;
begin
     n:=NumElemColumna(Player.Col);
     x:=Player.x;
     y:=Player.y;
     k:=1;
     if (player.pow=3)  {para que el laser no se mueva}
       then
         hay:=true
       else
         hay:=false;
     {Ciclo que controla cada elemento de la ficha
     con el de la derecha en la matriz}
     while (k<=n) and not hay do   {Si hay un elemento a la derecha es suficiente para que no podra moverse a la derecha}
       begin
         XYaIJ(x, y, i, j);     {calculamos las posiciones relativas de la ficha en la matriz}
         if Area[i,j+1]<>0  {El elemento a la derecha es distinto de cero?}
           then
             hay:=true;     {Entonces si hay un elemento a la derecha de este}
         inc(k, 1);         {Incrementamos el contador de elementos}
         inc(y, 10)         {Icrementamos la coordenada y para pasar al sig elemento}
       end;
     HayFDcha:=hay
end;

{Devuelve Verdadero si existe una ficha a la izq de la ficha que se mueve}
function HayFIzq(Player : Ficha; area : MatrizJuego) : boolean;
var
   hay        : boolean;
   k, n, i, j : byte;
   x, y       : word;
begin
     n:=NumElemColumna(PLayer.Col);
     x:=Player.x;
     y:=Player.y;
     k:=1;
     hay:=false;
     {Ciclo que controla cada elemento de la ficha
     con el de la izq en la matriz}
     while (k<=n) and not hay do   {Si hay un elemento a la izq es suficiente para que no podra moverse a la derecha}
       begin
         XYaIJ(x, y, i, j);   {calculamos las posiciones relativas de la ficha en la matriz}
         if Area[i,j-1]<>0  {El elemento de la izq es distinto de cero?}
           then
             hay:=true;     {Entonces si hay un elemento a la izq de este}
         inc(k, 1);         {Incrementamos el contador de elementos}
         inc(y, 10)         {Icrementamos la coordenada y para pasar al sig elemento}
       end;
     HayFIzq:=hay
end;

{Dibuja la ficha actual en la pantalla}
procedure DibujaFicha(Player : Ficha);
var
   i, j : byte;
begin
     {Algoritmo que lee el vector de la sig manera
     si lee un cero salta el espacio y no dibuja nada, sino dibuja la ficha
     correspondiente al numero leido}
     for i:=1 to 4 do
       if Player.Col[i]<>0
         then
           begin
             PonSpriteBSF(Player.x, Player.y, Player.Col[i], Sprites, Pant);
             inc(Player.y, 10)
           end
end;

{Comprueba si hay mas de 3 elementos del mismo tipo juntos y los elimina si hay}
procedure Coincidencias(i, j : byte; var Player : Ficha; var Area : MatrizJuego; var NCoin : byte);
type
    {Tipo de datos Pila, donde guarda las coordenadas de los elementos que se visitan}
    ElemPila = record
      i, j : byte;
    end;
    TPila = array[1..10] of ElemPila;

var
   Tope       : byte;
   Pila       : TPila;
{Devuelve si existe el elemento en la pila}
function ExisteEnPila(i, j :  byte) : boolean;
var
   e : boolean;
   p : byte;
begin
     p:=1;
     e:=false;
     while (p<=tope) and not e do
       if (pila[p].i=i) and (pila[p].j=j)
         then
           e:=true
         else
           inc(p, 1);
     ExisteEnPila:=e
end;

{Procedimiento recursivo que visita solamente los elementos iguales a el que
son adyacentes}
procedure Adyacentes(i, j : byte);
begin
     inc(tope, 1);
     pila[tope].i:=i;
     pila[tope].j:=j;
     if (i-1)>0 then
       if (Area[i-1, j]=Area[i,j]) and (Area[i,j]>0) and (Area[i,j]<26) and not ExisteEnPila(i-1, j)
         then
           adyacentes(i-1, j);
     if (j-1)>0 then
       if (Area[i, j-1]=Area[i,j]) and (Area[i,j]>0) and (Area[i,j]<26) and not ExisteEnPila(i, j-1)
         then
           adyacentes(i, j-1);
     if (i+1)<=maxi then
       if (Area[i+1, j]=Area[i,j]) and (Area[i,j]>0) and (Area[i,j]<26) and not ExisteEnPila(i+1, j)
         then
           adyacentes(i+1, j);
     if (j+1)<=maxj then
       if (Area[i, j+1]=Area[i,j]) and (Area[i,j]>0) and (Area[i,j]<26) and not ExisteEnPila(i, j+1)
         then
           adyacentes(i, j+1)
end;

{Procedimiento recursivo que borra los elementos marcados como adyacentes
en la pila}
procedure BorrarElem(i, j : byte);
var
   x, y : word;
begin
     area[i,j]:=0;
     if tope<>0
       then
         begin
           i:=pila[tope].i;
           j:=pila[tope].j;
           dec(tope, 1);
           BorrarElem(i,j)
         end
end;

{Anima las despariciones de las fichas leyendo desde la pila}
procedure AnimarDesapariciones;
var
   i, j, c, p, t : byte;
   x, y          : word;
begin
     for c:=0 to 2 do           {repite 3 veces porque es la cantidad de frames que posee la animacion}
       begin
         p:=1;
         while p<=tope do         {desapila usando un tope auxiliar}
           begin
             i:=pila[p].i;
             j:=pila[p].j;
             inc(p, 1);
             if area[i,j]<>0          {si el elemento es distinto de cero}
               then                   {le suma 6 frames para animar la desaparicion}
                 begin
                   inc(area[i,j], 6);
                   BorraPV(Pant, fondo);
                   DibujaPantalla(player, area);
                   Actualizar
                 end
           end
       end;

     {le pasamos las coordenadas de la pila de posiciones en la matriz a la de las coordenadas}
     for i:=1 to tope do
       IJaXY(pila[i].i, pila[i].j, pilaC[i].x, pilaC[i].y);
     LimYnum10:=pilac[1].y - 20;
     TopeC:=tope
end;

{Comienzo del subprograma Coincidencias}
begin
     tope:=0;
     adyacentes(i, j);
     if tope>=3
       then
         begin
           inc(player.combo, 1);
           PausaSMP(0);
           EliminaSample(sound);
           if BTFCargaSample('samples.btf', 'bweep.pcm', sound) = 0 then ReproduceSMP(sound, 11025);
           if (player.combo>=2)
             then
               begin
                 PausaSMP(0);
                 EliminaSample(sound);
                 if BTFCargaSample('samples.btf', 'combo.pcm', sound) = 0 then ReproduceSMP(sound, 11025);
                 randomize;
                 i:=random(3);
                 case i of
                 0 : inc(Player.laser, player.combo-1);
                 1 : inc(Player.desv, player.combo-1);
                 2 : inc(Player.bombas, player.combo-1)
                 end
               end;
           Player.puntos:=Player.puntos+(10*tope);
           AnimarDesapariciones;
           inc(NCoin, 1);
           i:=pila[tope].i;
           j:=pila[tope].j;
           dec(tope, 1);
           BorrarElem(i,j)
         end
end;

{Devuelve True si una columna de la matriz del juego esta vacia}
function ColMatVacia(j : byte; area : MatrizJuego) : boolean;
var
   i     : byte;
   vacia : boolean;
begin
     vacia:=true;
     i:=maxi;
     while (i>=1) and vacia do
       begin
         if area[i,j]<>0
           then
             vacia:=false;
         dec(i, 1);
       end;
     ColMatVacia:=vacia
end;

{Acumula todos los elementos de una columna abajo}
procedure ReorganizarCol(j : byte; var area : MatrizJuego);
var
   i, c, k : byte;
begin
     i:=maxi;
     c:=0;   {contador que nos servira para saber si comparo mas de 16 ceros}
     while (i>2) and (c<16) do
       begin
         if area[i,j]=0
           then
             begin
               inc(c, 1);
               for k:=i downto 2 do
                 area[k,j]:=area[k-1, j]
             end
           else
             dec(i, 1)
       end
end;

{Hace caer todos los elementos que esten "Flotando"}
procedure BajarElementos(var area : MatrizJuego);
var
   j : byte;
begin
     for j:=1 to maxj do       {si la col j no esta vacia la reorganizamos bajando}
       if not ColMatVacia(j, area)  { todos los elementos}
         then
           ReorganizarCol(j, area);
end;

{Mueve la ficha a la matriz del juego}
procedure MoverFichaArea(var Player : Ficha; var Area : MatrizJuego);
var
   n, i, j : byte;
   NCoin   : byte;
   uey     : word;
begin
     ReproduceSMP(toc, 11025);
     if player.pow=0
       then
         begin
           uey:=Player.y+(NumElemColumna(Player.Col)*10)-10;
           XYaIJ(Player.x, uey, i, j);
           if (Area[i,j]=0)                     {esta porcion del codigo comprueba que el ultimo}
             then                               {elemento de Player se copie sobre un elemento vacio de Area}
               XYaIJ(Player.x, Player.y, i, j)  {ya que ocurre sino un problema de "achatamiento" de un elemento de la matriz}
             else
               XYaIJ(Player.x, Player.y-10, i, j);
           for n:=1 to NumElemColumna(Player.Col) do
             begin
               Area[i,j]:=Player.Col[n];
               inc(i,1)
             end
         end
       else
         begin
           case player.pow of
           1 : begin
                 ExplosionBomba(player, area, Objs1);
                 BajarElementos(area)
               end;
           2 : begin
                 DesvanecerElementos(player, area, Objs1);
                 BajarElementos(area)
               end;
           3 : begin
                 DisparoLaser(player, area, objs1);
                 BajarElementos(area)
               end
           end
         end;
     repeat
       NCoin:=0;
       for i:=maxi downto 2 do
         for j:=1 to maxj do
           Coincidencias(i, j, Player, Area, NCoin); {Verifica si hay que eliminar elementos de la matriz}
        if NCoin>0 then BajarElementos(area);
     until NCoin=0;
end;

{Verifica todas las entradas por teclado, etc que se realizan en el juego}
procedure MoverFicha(var Player : Ficha; var area : MatrizJuego);
begin
     {Condiciones que manejan la repeticion del teclado cuando hay una
     tecla presionada (UltTecla)}
     if (not TPresionada) or (UltTecla>TiempoTecla)
       then
         begin
           UltTecla:=0;
           HTecla:=True
         end;

     if teclado[t_up] and HTecla
       then
         begin
           HTecla:=false;
           RotarArriba(Player.Col) {Rota la ficha hacia Arriba}
         end;

     if teclado[t_down] and HTecla
       then
         begin
           HTecla:=false;
           RotarAbajo(Player.Col)   {Rota la ficha hacia Abajo}
         end;

     if teclado[t_right] and not HayFDcha(Player, area) and (Player.x<LimDcho-10) and HTecla
       then
         begin
           HTecla:=false;
           inc(Player.x, 10)   {Mueve la ficha hacia la DERECHA}
         end;

     if teclado[t_left] and not HayFIzq(Player, area) and (Player.x>LimIzq) and HTecla
       then
         begin
           HTecla:=false;
           dec(Player.x, 10)   {Mueve la ficha hacia la IZQUIERDA}
         end;

     if teclado[t_acelera] and HTecla
       then
         begin
           HTecla:=false;
           inc(Player.y, 10)   {Acelera la ficha hacia ABAJO}
         end;

     if teclado[t_bomba] and (Player.Pow=0) and (Player.bombas>0) and HTecla
       then
         CreaBomba(Player);

     if teclado[t_desv] and (Player.Pow=0) and (Player.desv>0) and HTecla
       then
         CreaDesvanecedor(Player);

     if teclado[t_laser] and (Player.Pow=0) and (Player.laser>0) and HTecla
       then
         CreaLaser(Player);

     if teclado[t_shoot] and (player.pow=3) and HTecla
       then
         begin
           DisparoLaser(player, area, objs1);
           BajarElementos(area)
         end;

     if teclado[t_pausa] and HTecla then
       begin
         HTecla:=false;
         VaciaBuffer;
         repeat
           EscribeCent(Fnt, 3, 55,'Pausa', 126, TXT_Normal, Pant);
           Actualizar
         until teclado[t_pausa];
       end;
     {Controla el tiempo que hay que esperar para que la ficha avance}
     if Player.Time=TiempoFicha
       then
         begin
           inc(Player.y, 10);
           Player.Time:=0
         end
       else
         inc(Player.Time, 1);

     inc(UltTecla, 1)
end;

{Comprueba que la ficha no haya colisionado}
procedure CompruebaColisiones(var Player : Ficha; var area : MatrizJuego);
var
   i, j, k         : byte;
   x, y, uey       : word;
   colision, listo : boolean;
begin
     uey:=Player.y+(NumElemColumna(Player.Col)*10); {Calculamos la coordenada
                                                   "y" del ultimo elemento}
     if (uey > LimInf-1)     {Comprueba si choca contra el piso}
       then
         begin
           MoverFichaArea(Player, Area); {Si, entonces Mueve la ficha a la matriz}
           x:=Player.x;
           y:=Player.y;
           Player.Viva:=false  {Se debe crear otra ficha entonces, la ficha no esta viva}
         end
       else
         begin                 {No, entonces debe comprobar si choca contra otra ficha del area de juego}
           colision:=false;
           j:=1;               {iniciamos los indices de la matriz del area de juego}
           while (j<=maxj) and not colision do
             begin
               i:=(uey-10) div 10;  {i debe iniciarse en la coordenada del ultimo elemento}
               listo:=false;
               while (i<=maxi) and not colision and not listo do
                 begin
                   if area[i,j]<>0    {Si hay un elemento en la celda, comprobamos si hay colision}
                     then
                       begin
                         listo:=true;   {ya evaluamos la 1ra columna asi que pasemos a la siguiente}
                         {calculamos la coordenada y de la primera
                         ficha que encuentra leyendo de arriba hacia abajo}
                         IJaXY(i, j, x, y);
                         {Comprobamos si la coordenada del ultimo elemento
                         de la ficha colisiona con el de la matriz}
                         if (uey > y-1) and (abs(Player.x-x)=0)
                           then
                             begin
                               MoverFichaArea(Player, Area); {Si, entonces mandamos la ficha a la matriz}
                               Player.Viva:=false;
                               colision:=true     {hay colision, para que deje de comprobar}
                             end
                       end;
                   inc(i, 1)
                 end;
               inc(j, 1)
             end
         end;
       if colision and (player.y=20) then GameOver:=true
end;

{Realiza todo lo que se ve y se oye antes de empezar cada nivel}
procedure ReadyGo;
var
   i       : byte;
begin
     PausaSMP(0);
     EliminaSample(sound);
     if BTFCargaSample('samples.btf', 'jungle.pcm', sound) = 0 then ReproduceSMP(sound, 11025);

     i:=0;
     repeat
       BorraPV(Pant, fondo);
       DibujaPantalla(p1, areaj1);
       EscribeXY(Fnt, 125, 50, 'Level '+itos(nivel), 126, TXT_Normal, Pant);
       Actualizar;
       inc(i, 1);
     until i=50;
     i:=0;
     PausaSMP(0);
     EliminaSample(sound);
     if BTFCargaSample('samples.btf', 'readygo.pcm', sound) = 0 then ReproduceSMP(sound, 11025);
     repeat
       BorraPV(Pant, fondo);
       DibujaPantalla(p1, areaj1);
       EscribeXY(Fnt, 125, 50, 'Level '+itos(nivel), 126, TXT_Normal, Pant);
       EscribeXY(Fnt, 130, 60, 'Ready', 126, TXT_Normal, Pant);
       Actualizar;
       inc(i, 1);
     until i=50;
     i:=0;
     repeat
       BorraPV(Pant, fondo);
       DibujaPantalla(p1, areaj1);
       EscribeXY(Fnt, 140, 50, 'Go!', 126, TXT_Normal, Pant);
       Actualizar;
       inc(i, 1);
     until i=50
end;

{Realiza los calculos del Bonus y su presentacion en la pantalla}
procedure FinNivel;
var
   c, i   : byte;
   p_obj  : word;
   p_bomb : word;
   p_desv : word;
   p_laser: word;
   p_timer: word;
   total  : longint;
   obj    : array[1..5] of word;
   totals : string[10];
   puntos : string[10];
begin
     for c:=1 to NumObjs do
       obj[c]:=0;

     p_obj:=0;
     p_bomb:=P1.Bombas*50;
     p_laser:=P1.Laser*65;
     p_desv:=P1.desv*80;
     p_timer:=Timer*10;

     i:=0;
     if (objs1.num<>0)
       then
         begin
           PausaSMP(0);
           EliminaSample(sound);
           if BTFCargaSample('samples.btf', 'applause.pcm', sound) = 0 then ReproduceSMP(sound, 11025);
           repeat
             inc(i,1);
             BorraPV(Pant,fondo);
             EscribeXY(Fnt, 132, 50, 'Level', 126, TXT_Normal, Pant);
             EscribeXY(Fnt, 118, 60, 'Complete', 126, TXT_Normal, Pant);
             DibujaPantalla(P1, AreaJ1);
             Actualizar;
           until i=80;
         end
       else
         begin
           repeat
             inc(i, 1);
             BorraPV(Pant,fondo);
             EscribeXY(Fnt, 132, 50, 'Level', 126, TXT_Normal, Pant);
             EscribeXY(Fnt, 104, 60, 'Not Complete', 126, TXT_Normal, Pant);
             DibujaPantalla(P1, AreaJ1);
             Actualizar
           until i=80;
           i:=0;
           dec(P1.vidas, 1);
           if P1.vidas=255 then GameOver:=true;
           repeat
             inc(i, 1);
             BorraPV(Pant,fondo);
             DibujaPantalla(P1, AreaJ1);
             Actualizar
           until i=50;
         end;

     i:=0;
     repeat
       inc(i,1);
       BorraPV(Pant,0);
       EscribeXY(Fnt, 130, 30, 'Bonus', 126, TXT_Normal, Pant);

       for c:=1 to NumObjs do
          begin
            if not Objs1.arr[c].vivo
              then
                begin
                  PonSpriteBSF(26+(c*37), 55, Objs1.arr[c].tipo+1, Sprites, Pant);
                  EscribeXY(Fnt, 18+(c*37), 55, 'X', 126, TXT_Normal, Pant);
                end
              else
                begin
                  if i=50
                    then
                      begin
                        obj[c]:=1000-(200*Objs1.arr[c].danio);
                        p_obj:=p_obj+obj[c]
                      end;
                  PonSpriteBSF(26+(c*37), 55, Objs1.arr[c].tipo+Objs1.arr[c].danio, Sprites, Pant)
                end
          end;

       if i>50
         then
           begin
             for c:=1 to NumObjs do
               begin
                 if c=NumObjs
                   then
                     EscribeXY(Fnt, 1+(c*38), 70, itos(obj[c]), 126, TXT_Normal, Pant)
                   else
                     EscribeXY(Fnt, 1+(c*38), 70, itos(obj[c])+'+', 126, TXT_Normal, Pant);
                 EscribeXY(Fnt, 218, 70, ' = '+itos(p_obj), 126, TXT_Normal, Pant);
               end;
             EscribeXY(Fnt, 150, 88, '10 X '+Itos(Timer), 126, TXT_Normal, Pant);
             EscribeXY(Fnt, 218, 88, ' = '+itos(p_timer), 126, TXT_Normal, Pant);
             EscribeXY(Fnt, 150, 100, '50 X '+Itos(P1.bombas), 126, TXT_Normal, Pant);
             EscribeXY(Fnt, 218, 100, ' = '+itos(p_bomb), 126, TXT_Normal, Pant);
             EscribeXY(Fnt, 150, 112, '65 X '+Itos(P1.laser), 126, TXT_Normal, Pant);
             EscribeXY(Fnt, 218, 112, ' = '+itos(p_laser), 126, TXT_Normal, Pant);
             EscribeXY(Fnt, 150, 124, '80 X '+Itos(P1.desv), 126, TXT_Normal, Pant);
             EscribeXY(Fnt, 218, 124, ' = '+itos(p_desv), 126, TXT_Normal, Pant);
             total:=p_timer+p_bomb+p_desv+p_laser+p_obj;
             P1.Puntos:=P1.Puntos+total;
             str(total, totals);
             str(P1.Puntos, puntos);
             EscribeXY(Fnt, 178, 140, 'Total = '+totals, 126, TXT_Normal, Pant);
             EscribeXY(Fnt, 170, 165, 'Score = '+puntos, 2, TXT_Normal, Pant)
           end;

       EscribeXY(Fnt, 37, 88, 'Timer X '+Itos(Timer), 126, TXT_Normal, Pant);
       PonSpriteBSF(75, 99, 26, Sprites, Pant);
       EscribeXY(Fnt, 85, 100, 'X '+Itos(P1.bombas), 126, TXT_Normal, Pant);
       PonSpriteBSF(74, 111, 40, Sprites, Pant);
       EscribeXY(Fnt, 85, 112, 'X '+Itos(P1.laser), 126, TXT_Normal, Pant);
       PonSpriteBSF(74, 123, 39, Sprites, Pant);
       EscribeXY(Fnt, 85, 124, 'X '+Itos(P1.desv), 126, TXT_Normal, Pant);
       Actualizar;
     until i=51;

     repeat
     until TPresionada;

     if fondo=32 then fondo:=28 else inc(fondo, 1);
     inc(Nivel, 1);
     fin:=false;
     P1.Bombas:=0;
     P1.Laser:=0;
     P1.Desv:=0;
     P1.pow:=0;
     Timer:=120;
     SegAnt:=65535;
     combotime:=0;
     CreaAreaJuego(AreaJ1);
     if not gameover
       then
         begin
           BajarElementos(AreaJ1);
           ReadyGo;
           CreaFicha(P1)
         end
end;

{Realiza todo lo que se ve y se oye en el Game Over, incluyendo las inciales de Record si se lo ha alcanzado}
procedure JuegoTerminado;
var
   c   : byte;
   cad : string[3];
begin
     RectanguloSolido(LimIzq-20, 40, LimDcho+20, LimInf-50, fondo, Pant);
     Rectangulo(LimIzq-20, 40, LimDcho+20, LimInf-50, 9, Pant);
     if not hay_nivel
       then
         begin
           EscribeXY(Fnt, LimIzq+3, 95,'No Hay Mas', 126, TXT_Normal, Pant);
           EscribeXY(Fnt, LimIzq+3, 105,'Niveles', 126, TXT_Normal, Pant);
         end;
     EscribeXY(Fnt, LimIzq+3, 55,'Game', 126, TXT_Ampliado, Pant);
     EscribeXY(Fnt, LimIzq+3, 75,'Over', 126, TXT_Ampliado, Pant);
     Actualizar;
     PausaSMP(0);
     EliminaSample(sound);
     if BTFCargaSample('samples.btf', 'loss.pcm', sound) = 0 then ReproduceSMP(sound, 11025);
     for c:=0 to 25 do
       Actualizar;
     repeat
       VaciaBuffer;
     until TPresionada;
     if (HiScore.nombre = 'Hi ')
       then
         begin
           PausaSMP(0);
           EliminaSample(sound);
           if BTFCargaSample('samples.btf', 'applause.pcm', sound) = 0 then ReproduceSMP(sound, 11025);

           RectanguloSolido(LimIzq-20, 40, LimDcho+20, LimInf-50, fondo, Pant);
           Rectangulo(LimIzq-20, 40, LimDcho+20, LimInf-50, 9, Pant);
           EscribeCent(Fnt, 3, 55,'New Record!', 126, TXT_Normal, Pant);
           EscribeCent(Fnt, 3, 65,'Enter  Name', 126, TXT_Normal, Pant);
           EscribeXY(Fnt, 130, 85, chr(16), 10, TXT_Normal, Pant);
           EscribeXY(Fnt, 170, 85, chr(17), 10, TXT_Normal, Pant);
           Actualizar;
           c:=1;
           repeat
             DesinstalaTeclado;
             cad[c]:=readkey;
             cad[c]:=upcase(cad[c]);
             EscribeXY(Fnt, 130+(10*c), 85, cad[c], 126, TXT_Normal, Pant);
             inc(c, 1);
             Actualizar;
           until c=4;
           InstalaTeclado;
           repeat
           until Teclado[T_enter];
           HiScore.nombre:=cad;
           reset(HiRecord);
           write(HiRecord, HiScore);
           close(HiRecord);
         end
end;

{Realiza la animacion de las llamas consumiendo todoy elimina los objetos}
procedure AlFuego(var player : Ficha; var area : MatrizJuego; var objs : Objetos);
var
   i, j, c : byte;
begin
     player.viva:=false;

     for i:=1 to Objs.num do
       objs.arr[i].vivo:=false;
     Objs.num:=0;

     for c:=0 to 50 do
       begin
         BorraPV(Pant, fondo);
         EscribeXY(Fnt, 115, 60, 'Time Over', 126, TXT_Normal, Pant);
         DibujaPantalla(player, area);
         Actualizar
       end;

     PausaSMP(0);
     EliminaSample(sound);
     if BTFCargaSample('samples.btf', 'boom.pcm', sound) = 0 then ReproduceSMP(sound, 2500);
     c:=1;
     for i:=maxi downto 1 do
       begin
         BorraPV(Pant, fondo);
         DibujaPantalla(player, area);
         PonSpriteBSFAmpliado(LimIzq, LimInf-(c*15), framellama, Sprites, 100, 100*c, Pant);
         PonSpriteBSFAmpliado(LimIzq+24, LimInf-(c*15), framellama, Sprites, 100, 100*c, Pant);
         PonSpriteBSFAmpliado(LimIzq+49, LimInf-(c*15), framellama, Sprites, 100, 100*c, Pant);
         PonSpriteBSFAmpliado(LimIzq+73, LimInf-(c*15), framellama, Sprites, 100, 100*c, Pant);
         inc(c, 1);
         Actualizar;
         for j:=1 to maxj do
           area[i,j]:=0;
       end;
     dec(nivel, 1);
     fin:=true
end;

{Creditos}
procedure Credits;
begin
     PausaSMP(0);
     EliminaSample(sound);
     if BTFCargaSample('samples.btf', 'applause.pcm', sound) = 0 then ReproduceSMP(sound, 11025);
     BorraPV(Pant, 0);
     EscribeCent(Fnt, 3, 25,'Row & Col', 16, TXT_Normal, Pant);
     EscribeCent(Fnt, 3, 35,'CREDITS', 5, TXT_Normal, Pant);
     EscribeCent(Fnt, 3, 65,'Programacion : Agustin Meriles', 126, TXT_Normal, Pant);
     EscribeCent(Fnt, 3, 80,'Graficos     : Agustin Meriles', 126, TXT_Normal, Pant);
     EscribeCent(Fnt, 3, 95,'Sonidos      : Agustin Meriles', 126, TXT_Normal, Pant);
     EscribeCent(Fnt, 3, 110,'Voces        : Nicolas Meriles', 126, TXT_Normal, Pant);
     Actualizar;
     repeat
       VaciaBuffer;
     until TPresionada
end;

{Opciones}
procedure Options;
var
   pos_y, pos_x : byte;
   listo        : boolean;
   son          : byte;
begin
     listo:=false;
     pos_x:=40;
     pos_y:=50;
     son:=0;
     VaciaBuffer;
     repeat
       if (not TPresionada) then HTecla:=True;
       BorraPV(Pant, 0);
       EscribeXY(Fnt, 80, 20, 'Options', 16, TXT_Ampliado, Pant);
       EscribeXY(Fnt, 50, 50, 'Teclas', 5, TXT_Normal, Pant);
       EscribeXY(Fnt, 75, 60,  'Arriba            - '+NombreTecla(t_up), 125, TXT_Normal, Pant);
       EscribeXY(Fnt, 75, 70,  'Abajo             - '+NombreTecla(t_down), 125, TXT_Normal, Pant);
       EscribeXY(Fnt, 75, 80,  'Izquierda         - '+NombreTecla(t_left), 125, TXT_Normal, Pant);
       EscribeXY(Fnt, 75, 90, 'Derecha           - '+NombreTecla(t_right), 125, TXT_Normal, Pant);
       EscribeXY(Fnt, 75, 100, 'Sel. Bomba        - '+NombreTecla(t_bomba), 125, TXT_Normal, Pant);
       EscribeXY(Fnt, 75, 110, 'Sel. Desvanecedor - '+NombreTecla(t_desv), 125, TXT_Normal, Pant);
       EscribeXY(Fnt, 75, 120, 'Sel. Laser        - '+NombreTecla(t_laser), 125, TXT_Normal, Pant);
       EscribeXY(Fnt, 75, 130, 'Disparar Laser    - '+NombreTecla(t_shoot), 125, TXT_Normal, Pant);
       EscribeXY(Fnt, 75, 140, 'Acelerar Caida    - '+NombreTecla(t_acelera), 125, TXT_Normal, Pant);
       EscribeXY(Fnt, 75, 150, 'Pausa             - '+NombreTecla(t_pausa), 125, TXT_Normal, Pant);
       EscribeXY(Fnt, 50, 170, 'Sound Test', 5, TXT_Normal, Pant);
       EscribeXY(Fnt, 155, 170, 'WAVE '+itos(son), 125, TXT_Normal, Pant);
       EscribeXY(Fnt, pos_x, pos_y, chr(16), 15, TXT_Normal, Pant);

       case son of
       0 : EscribeXY(Fnt, 220, 170, 'Row & Col', 16, TXT_Normal, Pant);
       1 : EscribeXY(Fnt, 220, 170, 'Jungla', 16, TXT_Normal, Pant);
       2 : EscribeXY(Fnt, 220, 170, 'Ready Go!', 16, TXT_Normal, Pant);
       3 : EscribeXY(Fnt, 220, 170, 'Toc', 16, TXT_Normal, Pant);
       4 : EscribeXY(Fnt, 220, 170, 'Bweep!', 16, TXT_Normal, Pant);
       5 : EscribeXY(Fnt, 220, 170, 'Combo', 16, TXT_Normal, Pant);
       6 : EscribeXY(Fnt, 220, 170, 'Carga', 16, TXT_Normal, Pant);
       7 : EscribeXY(Fnt, 220, 170, 'BOOM!', 16, TXT_Normal, Pant);
       8 : EscribeXY(Fnt, 220, 170, 'Laser Shot!', 16, TXT_Normal, Pant);
       9 : EscribeXY(Fnt, 220, 170, 'Desvanece!', 16, TXT_Normal, Pant);
       10: EscribeXY(Fnt, 220, 170, 'Game Over', 16, TXT_Normal, Pant);
       11: EscribeXY(Fnt, 220, 170, 'Clap! Clap!', 16, TXT_Normal, Pant);
       end;

       if teclado[t_arriba] and (pos_x = 40) and HTecla then pos_y:=50;
       if teclado[t_abajo] and (pos_x = 40) and HTecla then pos_y:=170;
       if teclado[t_esc] and (pos_x = 40) and Htecla then listo:=true;
       if teclado[t_enter] and (pos_y = 50) and Htecla
         then
           begin
             HTecla:=false;
             pos_x:=65;
             pos_y:=60
           end;
       if teclado[t_enter] and (pos_y = 170) and HTecla
         then
           begin
             pos_x:=145;
             HTecla:=false
           end;

       if (pos_x=65)
         then
           begin
             if (teclado[t_abajo]) and (pos_y < 150) and Htecla
               then
                 begin
                   inc(pos_y, 10);
                   HTecla:=false;
                 end;
             if (teclado[t_arriba]) and (pos_y > 60) and Htecla
               then
                 begin
                   dec(pos_y, 10);
                   HTecla:=false;
                 end;
             if teclado[t_enter] and Htecla
               then
                 begin
                   EscribeXY(Fnt, 75, 190,  'Presiona una tecla...', 125, TXT_Normal, Pant);
                   Actualizar;
                   VaciaBuffer;
                   repeat
                   until TPresionada;
                   case pos_y of
                   60 : t_up:=LeeTecla;
                   70 : t_down:=LeeTecla;
                   80 : t_left:=LeeTecla;
                   90: t_right:=LeeTecla;
                   100: t_bomba:=LeeTecla;
                   110: t_desv:=LeeTecla;
                   120: t_laser:=LeeTecla;
                   130: t_shoot:=LeeTecla;
                   140: t_acelera:=LeeTecla;
                   150: t_pausa:=LeeTecla;
                   end;
                   VaciaBuffer;
                 end;
             if teclado[t_esc] and Htecla then
               begin
                 pos_x:=40;
                 pos_y:=50;
                 HTecla:=false;
               end;
           end;

       if (pos_x=145) then
         begin
           if teclado[t_esc] and Htecla then
             begin
               pos_x:=40;
               pos_y:=170;
               HTecla:=false;
             end;
           if teclado[t_arriba] and HTecla then
             begin
               if (son < 11) then inc(son, 1) else son:=0;
               HTecla:=false;
             end;
           if teclado[t_abajo] and HTecla then
             begin
               if (son > 0) then dec(son, 1) else son:=11;
               HTecla:=false;
             end;
           if teclado[t_enter]
             then
               case son of
               0  : begin
                      PausaSMP(0);
                      EliminaSample(sound);
                      BTFCargaSample('samples.btf', 'rowcol.pcm', sound);
                      ReproduceSMP(sound, 11025);
                    end;
               1  : begin
                      PausaSMP(0);
                      EliminaSample(sound);
                      BTFCargaSample('samples.btf', 'jungle.pcm', sound);
                      ReproduceSMP(sound, 11025);
                    end;
               2  : begin
                      PausaSMP(0);
                      EliminaSample(sound);
                      BTFCargaSample('samples.btf', 'readygo.pcm', sound);
                      ReproduceSMP(sound, 11025);
                    end;
               3  : begin
                      PausaSMP(0);
                      EliminaSample(sound);
                      BTFCargaSample('samples.btf', 'toc.pcm', sound);
                      ReproduceSMP(sound, 11025);
                    end;
               4  : begin
                      PausaSMP(0);
                      EliminaSample(sound);
                      BTFCargaSample('samples.btf', 'bweep.pcm', sound);
                      ReproduceSMP(sound, 11025);
                    end;
               5  : begin
                      PausaSMP(0);
                      EliminaSample(sound);
                      BTFCargaSample('samples.btf', 'combo.pcm', sound);
                      ReproduceSMP(sound, 11025);
                    end;
               6  : begin
                      PausaSMP(0);
                      EliminaSample(sound);
                      BTFCargaSample('samples.btf', 'gunload.pcm', sound);
                      ReproduceSMP(sound, 11025);
                    end;
               7  : begin
                      PausaSMP(0);
                      EliminaSample(sound);
                      BTFCargaSample('samples.btf', 'boom.pcm', sound);
                      ReproduceSMP(sound, 11025);
                    end;
               8  : begin
                      PausaSMP(0);
                      EliminaSample(sound);
                      BTFCargaSample('samples.btf', 'laser.pcm', sound);
                      ReproduceSMP(sound, 11025);
                    end;
               9  : begin
                      PausaSMP(0);
                      EliminaSample(sound);
                      BTFCargaSample('samples.btf', 'desv.pcm', sound);
                      ReproduceSMP(sound, 11025);
                    end;
               10 : begin
                      PausaSMP(0);
                      EliminaSample(sound);
                      BTFCargaSample('samples.btf', 'loss.pcm', sound);
                      ReproduceSMP(sound, 11025);
                    end;
               11 : begin
                      PausaSMP(0);
                      EliminaSample(sound);
                      BTFCargaSample('samples.btf', 'applause.pcm', sound);
                      ReproduceSMP(sound, 11025);
                    end;
               end;
         end;
       Actualizar;
     until listo;
end;

{Realiza todo lo que se ve y se oye en la Pantalla de Inicio}
procedure PantallaInicio;
var
   i, pos_y : byte;
   selec    : boolean;
begin
     repeat
       for i:=7 downto 1 do
         begin
           BorraPV(Pant, 8);
           PonSpriteBSFAmpliado(125, 50, 46, Sprites, 100*i, 100*i, Pant);
           Actualizar
         end;
       for i:=7 downto 1 do
         begin
           BorraPV(Pant, 8);
           PonSpriteBSF(125, 50, 46, Sprites, Pant);
           PonSpriteBSFAmpliado(142, 75, 47, Sprites, 100*i, 100*i, Pant);
           Actualizar
         end;
       for i:=7 downto 1 do
         begin
           BorraPV(Pant, 8);
           PonSpriteBSF(125, 50, 46, Sprites, Pant);
           PonSpriteBSF(142, 75, 47, Sprites, Pant);
           PonSpriteBSFAmpliado(135, 100, 48, Sprites, 100*i, 100*i, Pant);
           Actualizar
         end;
       for i:=50 downto 20 do
         begin
           BorraPV(Pant, 8);
           PonSpriteBSF(125, i, 46, Sprites, Pant);
           PonSpriteBSF(142, 25+i, 47, Sprites, Pant);
           PonSpriteBSF(135, 50+i, 48, Sprites, Pant);
           Actualizar
         end;

       selec:=false;
       pos_y:=125;

       PausaSMP(0);
       EliminaSample(sound);
       if BTFCargaSample('samples.btf', 'rowcol.pcm', sound) = 0 then ReproduceSMP(sound, 11025);

       repeat
         BorraPV(Pant, 0);
         PonSpriteBSF(125, i, 46, Sprites, Pant);
         PonSpriteBSF(142, 25+i, 47, Sprites, Pant);
         PonSpriteBSF(135, 50+i, 48, Sprites, Pant);
         EscribeXY(Fnt, 120, 125, 'Start', 5, TXT_Normal, Pant);
         EscribeXY(Fnt, 120, 140, 'Options', 126, TXT_Normal, Pant);
         EscribeXY(Fnt, 120, 155, 'Credits', 16, TXT_Normal, Pant);
         EscribeXY(Fnt, 110, pos_y, chr(16), 15, TXT_Normal, Pant);
         EscribeXY(Fnt, 80, 180, '2002 - JAM Software', 126, TXT_Normal, Pant);
         Actualizar;
         if teclado[t_esc] then salir:=true;
         if teclado[t_arriba] and (pos_y > 125) then dec(pos_y, 15);
         if teclado[t_abajo] and (pos_y < 155) then inc(pos_y, 15);
         if teclado[t_enter] then selec:=true;
         VaciaBuffer;
      until selec or salir;

      if (pos_y = 140) and not salir then Options;
      if (pos_y = 155) and not salir then Credits;
     until (pos_y = 125) or salir
end;

procedure Finalizaciones;   {Pone todo como estaba antes de empezar el juego}
begin
     EliminaSample(sound);
     EliminaSample(toc);
     EliminaBSF(Sprites);
     DesinstalaTeclado;
     FinalizaUgo
end;

begin
     salir:=false;
     Inicializaciones;
     PantallaInicio;
     while (not salir) do begin
       NuevoJuego;
       CreaAreaJuego(AreaJ1);
       BajarElementos(AreaJ1);
       ReadyGo;
       CreaFicha(P1);
       CreaFicha(P1);
       repeat
         if (P1.puntos > HiScore.puntos)
           then
             begin
               HiScore.puntos:=P1.Puntos;
               HiScore.nombre:='Hi '
             end;
         if Timer=0 then AlFuego(P1, AreaJ1, Objs1);
         if fin
           then
             FinNivel
           else
             begin
               MoverFicha(P1, AreaJ1);
               if not P1.Viva then CreaFicha(P1); {Si la ficha no esta viva, crea otra}
               CompruebaColisiones(P1, AreaJ1);
               BorraPV(Pant,fondo);
               DibujaFicha(P1);
               DibujaPantalla(P1, AreaJ1);
               Actualizar;
             end;
         if LlegaronObjetos(Objs1, AreaJ1) then Fin:=true;
       until GameOver;
       JuegoTerminado;
       PantallaInicio;
     end;
     Finalizaciones
end.
