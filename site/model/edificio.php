<?php
//Este ficheiro vai ter todas as funcoes que comunicam com a db
// e transforma em dados usados pelo resto do programa
//por uma questao de simplicidade, acho que faz mais sentido lidarmos com
//atualizacoes a db dentro do proprio endpoint na api (porque fica mais facil de ler o codigo)
function edificio_getall($db) {
	$query = "SELECT * FROM edificio";
 	return $db->query($query);
}

function edificio_get_alugaveis_total($db, $morada){
	$query_model = "SELECT morada, codigo, SUM(custo) as total
			FROM 
			(
				SELECT morada, codigo, tarifa*DATEDIFF(data_fim,data_inicio) as custo
				FROM espaco NATURAL JOIN oferta NATURAL JOIN aluga NATURAL JOIN paga
				WHERE YEAR(data) = '2016'
				UNION
				SELECT morada, codigo_espaco AS codigo, tarifa*DATEDIFF(data_fim,data_inicio) as custo
 				FROM posto NATURAL JOIN oferta NATURAL JOIN aluga NATURAL JOIN paga
 				WHERE YEAR(data) = '2016'
			) as all_custos
			WHERE morada=:morada
			GROUP BY morada, codigo;";
	$prepared_query = $db->prepare($query_model);
	$prepared_query->bindParam(":morada", $morada);
	$prepared_query->execute();
	return $prepared_query;
}
?>
