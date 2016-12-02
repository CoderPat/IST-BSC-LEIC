<?php
require_once "../../func/init.php";
include $root . "model/reserva.php";
include $root . "view/table.php";
$title = "Listar Reserva";

include $root . "template/head.php"; 
include $root . "template/navbar.php";
$table = reserva_getall($db);
?>

<link rel="stylesheet" type="text/css" href="<?= $webroot ?>/assets/css/tabler.css">

 <div class="jumbotron">
    <div class="container">  
        <h3>
            Criar Reserva
        </h3>
        <form class="betterform" action=<?= $webroot . "/api/reserva.php" . "?redirect=" . $webroot . "pages/reserva/"?> method="post">
            <label for="input-morada">Morada do Alugavel:</label>
            <input type="text" name="morada" id="input-morada"/>
            <label for="input-codigo">Codigo do alugavel:</label>
            <input type="text" name="codigo" id="input-codigo"/>
            <label for="input-data_inicio">Data de inicio da Oferta: </label>
            <input type="text" name="data_inicio" id="input-data_inicio"/>
            <label for="input-nif">NIF do Utilizador Criador: </label>
            <input type="text" name="nif" id="input-nif"/>
            <input type="submit" value="Submit"/>
        </form>
    </div>
</div>


<div class="container">

<?php
function make_request_btn($numero) {
    global $webroot;
    return '<td class="table-buttons" style="font-size: 1.5em; padding: 10px 10px 0 10px;">'.
    "<form action='$webroot/api/edificio.php?callback=".urlencode("$webroot/pages/edificio")."' method='POST'>".
    '<input type="hidden" name="_method" value="DELETE" />'.
    '<input type="hidden" name="numero" value="'.htmlspecialchars($numero, ENT_QUOTES, 'UTF-8').'" />'.
    '<button type="submit" data-original-title="Pagar Reserva" data-placement="bottom" data-toggle="tooltip"'.
    'class="tooltipper" class="btn btn-xs btn-danger"><i class="glyphicon glyphicon-usd"></i></button>'.
    '</form></td>';
}
    draw_table($table, "Lista de Reservas", null, ["Numero", "Accoes"], [null, 'make_request_btn'], [["numero"],["numero"]], null);
?>

    

</div>
<?php
include $root . "template/foot.php";
?>