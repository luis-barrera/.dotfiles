Vim�UnDo� Wf��ح&]��*�E����H��Z�<�	Y��  "                ?       ?   ?   ?    _��f    _�                     "        ����                                                                                                                                                                                                                                                                                                                                                             _���     �   "            5�_�                    #        ����                                                                                                                                                                                                                                                                                                                                                             _���     �   #               �   $            �   "               5�_�                    #        ����                                                                                                                                                                                                                                                                                                                                                             _���     �   "   %   ;       5�_�                    $   +    ����                                                                                                                                                                                                                                                                                                                                                             _���     �   #   %   <      +-- funcion para agregar entradas a la tabla5�_�                    <       ����                                                                                                                                                                                                                                                                                                                                                             _���     �   ;              delimiter ;5�_�                    =        ����                                                                                                                                                                                                                                                                                                                                                             _���     �   <               5�_�                    >   &    ����                                                                                                                                                                                                                                                                                                                                                             _���     �   =              &-- para hacerlo en todas los renglones5�_�      	              ?        ����                                                                                                                                                                                                                                                                                                                                                             _���    �   >               �   ?            5�_�      
           	   <   
    ����                                                                                                                                                                                                                                                                                                                            <   
       %           v        _���     �   ;   P   J      delimiter ;�   <   =   J    5�_�   	              
   <       ����                                                                                                                                                                                                                                                                                                                            <   
       %           v        _���     �   ;   >   ]      Cdelimiter ;CREATE DEFINER=`root`@`localhost` PROCEDURE `setSesion`(5�_�   
                 =        ����                                                                                                                                                                                                                                                                                                                            <   
       %           v        _���     �   <   ?   ^      8CREATE DEFINER=`root`@`localhost` PROCEDURE `setSesion`(5�_�                    %        ����                                                                                                                                                                                                                                                                                                                            <   
       %           v        _���     �   $   &   _      delimiter $$       create procedure setSesion(     in mat int   )   begin   !  declare inicio, final DATETIME;   #  declare inicio2, final2 DATETIME;   #  declare inicio3, final3 DATETIME;        G  SELECT now() - interval FLOOR(30 + (RAND() * 10)) MINUTE into inicio;   F  SELECT now() + interval FLOOR(30 + (RAND() * 10)) MINUTE into final;   O  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio, final);        H  SELECT now() - interval FLOOR(30 + (RAND() * 10)) MINUTE into inicio2;   G  SELECT now() + interval FLOOR(30 + (RAND() * 10)) MINUTE into final2;   O  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio, final);        H  SELECT now() - interval FLOOR(30 + (RAND() * 10)) MINUTE into inicio3;   G  SELECT now() + interval FLOOR(30 + (RAND() * 10)) MINUTE into final3;   O  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio, final);   end $$       delimiter ;5�_�                    %        ����                                                                                                                                                                                                                                                                                                                            %   
       %           v        _���     �   $   %           5�_�                    %        ����                                                                                                                                                                                                                                                                                                                            %   
       %           v        _���    �   $   %           5�_�                    8       ����                                                                                                                                                                                                                                                                                                                            9           %           v        _��g     �   8   :   F    5�_�                    9        ����                                                                                                                                                                                                                                                                                                                            :           %           v        _��g     �   8   ;   G       5�_�                    :        ����                                                                                                                                                                                                                                                                                                                            ;           %           v        _��g     �   :   S   I       �   ;   <   I    �   9   <   H       5�_�                    9        ����                                                                                                                                                                                                                                                                                                                            R          9           v        _��v     �   8   \   F       �   9   :   F    �   8   :   `              delimiter $$       CREATE PROCEDURE setSesion(     in mat int   )   begin   !  declare inicio, final DATETIME;   #  declare inicio2, final2 DATETIME;   #  declare inicio3, final3 DATETIME;        G  SELECT now() - interval FLOOR(30 + (RAND() * 10)) MINUTE into inicio;   F  SELECT now() + interval FLOOR(30 + (RAND() * 10)) MINUTE into final;   O  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio, final);        H  SELECT now() - interval FLOOR(30 + (RAND() * 10)) MINUTE into inicio2;   G  SELECT now() + interval FLOOR(30 + (RAND() * 10)) MINUTE into final2;   Q  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio2, final2);        H  SELECT now() - interval FLOOR(30 + (RAND() * 10)) MINUTE into inicio3;   G  SELECT now() + interval FLOOR(30 + (RAND() * 10)) MINUTE into final3;   Q  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio3, final3);   end $$       delimiter ;    5�_�                    8       ����                                                                                                                                                                                                                                                                                                                            9           [   
       v        _��y     �   7   :   h      end5�_�                    \       ����                                                                                                                                                                                                                                                                                                                            :           \   
       v        _��|    �   [   ^   i      delimiter ;5�_�                    :        ����                                                                                                                                                                                                                                                                                                                            ]           :           v        _���     �   9   k   F      &-- para hacerlo en todas los renglones�   :   ;   F    �   9   ;   j   %   delimiter $$       CREATE PROCEDURE setSesion(     in mat int   )   begin   !  declare inicio, final DATETIME;   #  declare inicio2, final2 DATETIME;   #  declare inicio3, final3 DATETIME;     declare x, y int;        *  select FLOOR(30 + (RAND() * 10)) into x;   *  select FLOOR(30 + (RAND() * 10)) into y;     if x < 35 then       set incio = null;     ELSE   I    SELECT now() - interval FLOOR(30 + (RAND() * 10)) MINUTE into inicio;   	  end if;     if x < 35 then       set incio = null;     else   H    SELECT now() + interval FLOOR(30 + (RAND() * 10)) MINUTE into final;   	  end if;   O  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio, final);        H  SELECT now() - interval FLOOR(30 + (RAND() * 10)) MINUTE into inicio2;   G  SELECT now() + interval FLOOR(30 + (RAND() * 10)) MINUTE into final2;   Q  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio2, final2);        H  SELECT now() - interval FLOOR(30 + (RAND() * 10)) MINUTE into inicio3;   G  SELECT now() + interval FLOOR(30 + (RAND() * 10)) MINUTE into final3;   Q  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio3, final3);   end $$       delimiter ;       &-- para hacerlo en todas los renglones5�_�                    j       ����                                                                                                                                                                                                                                                                                                                            :           j   
       v        _���     �   i   l   v      1delimiter ;-- para hacerlo en todas los renglones5�_�                    k        ����                                                                                                                                                                                                                                                                                                                            :           j   
       v        _���     �   j   m   w      &-- para hacerlo en todas los renglones5�_�                    l        ����                                                                                                                                                                                                                                                                                                                            :           j   
       v        _���     �   k   n   x      &-- para hacerlo en todas los renglones5�_�                    m        ����                                                                                                                                                                                                                                                                                                                            :           j   
       v        _���    �   l   o   y      &-- para hacerlo en todas los renglones5�_�                    :        ����                                                                                                                                                                                                                                                                                                                            j   
       :           v        _���    �   9   x   J       �   :   ;   J    �   9   ;   z   1   delimiter $$       CREATE PROCEDURE setSesion(     in mat int   )   begin   !  declare inicio, final DATETIME;   #  declare inicio2, final2 DATETIME;   #  declare inicio3, final3 DATETIME;     declare x, y int;        *  select FLOOR(30 + (RAND() * 10)) into x;   *  select FLOOR(30 + (RAND() * 10)) into y;     if x < 35 then       set incio = null;     ELSE   1    SELECT now() - interval x MINUTE into inicio;   	  end if;     if y < 35 then       set final = null;     else   0    SELECT now() + interval y MINUTE into final;   	  end if;   O  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio, final);        *  select FLOOR(30 + (RAND() * 10)) into x;   *  select FLOOR(30 + (RAND() * 10)) into y;     if x < 35 then       set incio = null;     ELSE   1    SELECT now() - interval x MINUTE into inicio;   	  end if;     if y < 35 then       set final = null;     else   0    SELECT now() + interval y MINUTE into final;   	  end if;   O  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio, final);        H  SELECT now() - interval FLOOR(30 + (RAND() * 10)) MINUTE into inicio2;   G  SELECT now() + interval FLOOR(30 + (RAND() * 10)) MINUTE into final2;   Q  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio2, final2);        H  SELECT now() - interval FLOOR(30 + (RAND() * 10)) MINUTE into inicio3;   G  SELECT now() + interval FLOOR(30 + (RAND() * 10)) MINUTE into final3;   Q  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio3, final3);   end $$       delimiter ;5�_�                    :        ����                                                                                                                                                                                                                                                                                                                            w   
       :           v        _���    �   9   x   J       �   :   ;   J    �   9   ;   �   >   delimiter $$       CREATE PROCEDURE setSesion(     in mat int   )   begin   !  declare inicio, final DATETIME;   #  declare inicio2, final2 DATETIME;   #  declare inicio3, final3 DATETIME;     declare x, y int;          -- examen 1   *  select FLOOR(30 + (RAND() * 10)) into x;   *  select FLOOR(30 + (RAND() * 10)) into y;     if x < 35 then       set incio = null;     ELSE   .    select now() - interval 40 day into incio;   1    SELECT incio - interval x MINUTE into inicio;   	  end if;     if y < 35 then       set final = null;     else   1    SELECT inicio + interval y MINUTE into final;   	  end if;   O  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio, final);          -- examen 2   *  select FLOOR(30 + (RAND() * 10)) into x;   *  select FLOOR(30 + (RAND() * 10)) into y;     if x < 37 then       set incio = null;     ELSE   .    select now() - interval 20 day into incio;   2    SELECT inicio - interval x MINUTE into inicio;   	  end if;     if y < 37 then       set final = null;     else   0    SELECT incio + interval y MINUTE into final;   	  end if;   O  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio, final);            -- examen 3   *  select FLOOR(30 + (RAND() * 10)) into x;   *  select FLOOR(30 + (RAND() * 10)) into y;     if x < 40 then       set incio = null;     ELSE   .    select now() - interval 40 day into incio;   1    SELECT incio - interval x MINUTE into inicio;   	  end if;     if y < 40 then       set final = null;     else   1    SELECT inicio + interval y MINUTE into final;   	  end if;   O  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio, final);       end $$       delimiter ;5�_�                    :        ����                                                                                                                                                                                                                                                                                                                            k          :           v        _��0    �   9   v   V      "w() - interval 40 day into inicio;�   :   ;   V    �   9   ;   �   2   delimiter $$       CREATE PROCEDURE setSesion(     in mat int   )   begin   !  declare inicio, final DATETIME;   #  declare inicio2, final2 DATETIME;   #  declare inicio3, final3 DATETIME;     declare x, y int;          -- examen 1   *  select FLOOR(30 + (RAND() * 10)) into x;   *  select FLOOR(30 + (RAND() * 10)) into y;     if x < 35 then       set inicio = null;     ELSE   /    select now() - interval 40 day into inicio;   2    SELECT inicio - interval x MINUTE into inicio;   	  end if;     if y < 35 then       set final = null;     else   1    SELECT inicio + interval y MINUTE into final;   	  end if;   O  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio, final);          -- examen 2   *  select FLOOR(30 + (RAND() * 10)) into x;   *  select FLOOR(30 + (RAND() * 10)) into y;     if x < 37 then       set inicio = null;     ELSE   /    select now() - interval 20 day into inicio;   2    SELECT inicio - interval x MINUTE into inicio;   	  end if;     if y < 37 then       set final = null;     else   0    SELECT incio + interval y MINUTE into final;   	  end if;   O  insert into Sesión (Matricula, tInicio, tFinal) values (mat, inicio, final);            -- examen 3   *  select FLOOR(30 + (RAND() * 10)) into x;   *  select FLOOR(30 + (RAND() * 10)) into y;     if x < 40 then       set inicio = null;     ELSE   /    select now() - interval 40 day into inicio;5�_�                    �   
    ����                                                                                                                                                                                                                                                                                                                            :           u   
       v        _��C     �   �            5�_�                    �        ����                                                                                                                                                                                                                                                                                                                            :           u   
       v        _��C     �   �               5�_�                     �        ����                                                                                                                                                                                                                                                                                                                            :           u   
       v        _��C     �   �               �   �            �   �               5�_�      !               �        ����                                                                                                                                                                                                                                                                                                                            :           u   
       v        _��F    �   �   �   �       5�_�       "           !   �   
    ����                                                                                                                                                                                                                                                                                                                            :           u   
       v        _���     �   �               �   �            5�_�   !   #           "   �   
    ����                                                                                                                                                                                                                                                                                                                            :           u   
       v        _���     �   �               �   �            �   �              
-- funcion5�_�   "   $           #   �        ����                                                                                                                                                                                                                                                                                                                            :           u   
       v        _���     �   �            5�_�   #   %           $   �        ����                                                                                                                                                                                                                                                                                                                            :           u   
       v        _���   	 �   �               5�_�   $   &           %   �        ����                                                                                                                                                                                                                                                                                                                                ]                  v       _��A     �   �            5�_�   %   '           &   �        ����                                                                                                                                                                                                                                                                                                                                ]                  v       _��A     �   �               �   �            �   �               5�_�   &   (           '   �        ����                                                                                                                                                                                                                                                                                                                                ]                  v       _��D     �   �   �   �       5�_�   '   )           (   �       ����                                                                                                                                                                                                                                                                                                                                ]                  v       _��M     �   �              delimiter ;5�_�   (   *           )   �        ����                                                                                                                                                                                                                                                                                                                                ]                  v       _��M   
 �   �               �   �            �   �               5�_�   )   +           *   �   
    ����                                                                                                                                                                                                                                                                                                                                ]                  v       _���     �   �            5�_�   *   ,           +   �        ����                                                                                                                                                                                                                                                                                                                                ]                  v       _���     �   �               5�_�   +   /           ,   �        ����                                                                                                                                                                                                                                                                                                                                ]                  v       _���     �   �               �   �            �   �               5�_�   ,   0   -       /   u       ����                                                                                                                                                                                                                                                                                                                                ]                  v       _���     �   t   w   �      -delimiter ;w() - interval 40 day into inicio;5�_�   /   1           0   v        ����                                                                                                                                                                                                                                                                                                                                ]                  v       _���    �   u   x   �      "w() - interval 40 day into inicio;5�_�   0   2           1   �        ����                                                                                                                                                                                                                                                                                                                                           Z       v   Z    _�j     �   �            5�_�   1   3           2   �        ����                                                                                                                                                                                                                                                                                                                                           Z       v   Z    _�j     �   �               5�_�   2   4           3   �        ����                                                                                                                                                                                                                                                                                                                                           Z       v   Z    _�j     �   �   �   �      select�   �               �   �            �   �               5�_�   3   5           4   �   !    ����                                                                                                                                                                                                                                                                                                                                           Z       v   Z    _�p     �   �              !where Sesión.tFinal is not null;5�_�   4   6           5   �        ����                                                                                                                                                                                                                                                                                                                                           Z       v   Z    _�p     �   �               5�_�   5   7           6          ����                                                                                                                                                                                                                                                                                                                               s                 v       _���     �         �      r-- INSERT INTO `examen`.`BancoRespuestas` (`idPregunta`, `descRespuesta`) VALUES ('1', 'Un valor de la realidad');5�_�   6   9           7   �        ����                                                                                                                                                                                                                                                                                                                               s                 v       _���    �   �               �   �            5�_�   7   :   8       9   �   
    ����                                                                                                                                                                                                                                                                                                                               s                 v       _��     �   �            5�_�   9   ;           :   �        ����                                                                                                                                                                                                                                                                                                                               s                 v       _��     �   �               5�_�   :   <           ;   �        ����                                                                                                                                                                                                                                                                                                                               s                 v       _��    �   �               �   �            �   �               5�_�   ;   =           <          ����                                                                                                                                                                                                                                                                                                                               s                 v       _��e     �              5�_�   <   >           =  	        ����                                                                                                                                                                                                                                                                                                                               s                 v       _��e     �                 5�_�   =   ?           >  
        ����                                                                                                                                                                                                                                                                                                                               s                 v       _��e     �  	               5�_�   >               ?          ����                                                                                                                                                                                                                                                                                                                               s                 v       _��e    �                 �              �  
               5�_�   7           9   8   �        ����                                                                                                                                                                                                                                                                                                                               s                 v       _���     �   �   �   �    �   �   �   �      ddelimiter $$       create procedure cal(   	  ses int   )   begin     declare cal int;     select count(*) into cal     from Examen     INNER join BancoPreguntas     using(idPregunta)   (  where (Respuesta = RespuestaCorrrecta)     and (idSesion = ses);        ;  update Sesión set Aciertos = cal where (idSesion = ses);   end $$       delimiter ;elimiter $$5�_�   ,   .       /   -   �        ����                                                                                                                                                                                                                                                                                                                                ]                  v       _���     �   �   �           5�_�   -               .   �        ����                                                                                                                                                                                                                                                                                                                            <           Y          v       _���    �   �   �               5��