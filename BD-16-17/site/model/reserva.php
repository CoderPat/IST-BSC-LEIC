<?php
//Este ficheiro vai ter todas as funcoes que comunicam com a db
// e transforma em dados usados pelo resto do programa
//por uma questao de simplicidade, acho que faz mais sentido lidarmos com
//atualizacoes a db dentro do proprio endpoint na api (porque fica mais facil de ler o codigo)
function reserva_getall($db) {
	$query = "SELECT numero, estado
			  FROM ( SELECT numero, MAX(time_stamp) as max  
					FROM ( SELECT * FROM reserva NATURAL JOIN estado) as all_states GROUP BY numero ) as max_timestamp NATURAL JOIN estado 
			  WHERE time_stamp=max;";
 	return $db->query($query);
}

?>