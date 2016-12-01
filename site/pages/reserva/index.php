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
        <form action=<?= $webroot . "/api/reserva.php" . "?redirect=" . $webroot . "pages/reserva/"?> method="post">
            <p>Morada do Alugavel: <input type="text" name="morada"/></p>
            <p>Codigo do Espaco: <input type="text" name="codigo"/></p>
            <p>Data inicial da Oferta: <input type="text" name="data_inicio"/></p>
            <p>Nif do Utilizador: <input type="text" name="nif"/></p>
            <p><input type="submit" value="Submit"/></p>
        </form>
    </div>
</div>


<div class="container">

<?php
function make_request_btn($numero) {
    global $webroot;
    return '<td style="font-size: 1.5em; padding: 10px 10px 0 10px;">'.
    '<button type="submit" data-original-title="Apagar oferta" data-placement="bottom" data-toggle="tooltip"'.'class="tooltipper" class="btn btn-xs btn-danger"> <span class="glyphicon glyphicon-usd"></span>&nbsp; </button>'.
    '</form></td>';
}
    draw_table($table, "Lista de Reservas", null, ["Numero", "Accoes"], [null, 'make_request_btn'], [["numero"],["numero"]], null);
?>

    

</div>
<?php
include $root . "template/foot.php";
?>