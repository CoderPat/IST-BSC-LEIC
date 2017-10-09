<?php
//Este ficheiro vai ter todas as funcoes que comunicam com a db
// e transforma em dados usados pelo resto do programa
//por uma questao de simplicidade, acho que faz mais sentido lidarmos com
//atualizacoes a db dentro do proprio endpoint na api (porque fica mais facil de ler o codigo)
function espaco_getall($db) {
	$query = "SELECT * FROM espaco";
 	return $db->query($query);
}

?>