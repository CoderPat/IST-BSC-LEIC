<?php
require_once "../../func/init.php";
$title = "Ver edifício";
include $root . "template/head.php"; 
include $root . "template/navbar.php";

if (isset($_GET['morada']) && !empty($_GET['morada'])) {
    $morada = int($_GET['morada']);
} else {
    $morada = 0;
}

$edificio = edificio_get($morada); //Isto retorna um objeto de edificio ja sem estar incorporado em PDO
//depois usa-se diretamente aqui o $edificio para escrever o que for preciso
$alugaveis = alugavel_getall_by_morada($morada);
?>


<?
draw_table($alugaveis);
//TODO: por aqui todas as outras tables que têm  de ser desenhadas para isto

//Faz sentido a funcao para criar espacos estar aqui e nao em nenhum outro lado

//Incluir aqui tambem a lista de ofertas sobre este espaco.

//E a lista de reservas
?>



<?php
include $root . "template/foot.php";
?>