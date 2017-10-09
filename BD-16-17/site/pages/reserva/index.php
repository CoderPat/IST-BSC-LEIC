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
        <form class="betterform" action=<?= $webroot . "/api/reserva.php" . "?callback=" . $webroot . "/pages/reserva/"?> method="post">
            <label for="input-morada">Morada do Alugavel:</label>
            <input type="text" name="morada" id="input-morada" <?=isset($_POST['morada']) ? "value=\"".htmlspecialchars($_POST['morada'], ENT_QUOTES, 'UTF-8')."\"" : ""?> />
            <label for="input-codigo">Codigo do alugavel:</label>
            <input type="text" name="codigo" id="input-codigo" <?=isset($_POST['codigo']) ? "value=\"".htmlspecialchars($_POST['codigo'], ENT_QUOTES, 'UTF-8')."\"" : ""?> />
            <label for="input-data_inicio">Data de inicio da Oferta: </label>
            <input type="date" name="data_inicio" id="input-data_inicio" <?=isset($_POST['data_inicio']) ? "value=\"".htmlspecialchars($_POST['data_inicio'], ENT_QUOTES, 'UTF-8')."\"" : ""?> />
            <label for="input-nif">NIF do Utilizador Criador: </label>
            <input type="text" name="nif" id="input-nif" value="123456719"/>
            <label for="input-numero">Numero da reserva: </label>
            <input type="text" name="numero" id="input-numero"/>
            <label for="input-estado">Estado: </label>
            <input type="text" name="estado" id="input-estado"/>
            <input type="submit" value="Criar"/>
        </form>
    </div>
    <div class="container">  
        <h3>
            Pagar Reserva
        </h3>
        <form class="betterform" action=<?= $webroot . "/api/reserva.php" . "?callback=" . $webroot . "/pages/reserva/"?> method="post">
            <label for="input-numero">Numero da reserva: </label>
            <input type="text" name="numero" id="input-numero" <?=isset($_POST['numero']) ? "value=\"".htmlspecialchars($_POST['numero'], ENT_QUOTES, 'UTF-8')."\"" : ""?> />
            <label for="input-metodo">Metodo de Pagamento: </label>
            <input type="text" name="metodo" id="input-metodo"/>
            <label for="input-data">Data de Pagamento: </label>
            <input type="date" name="data" id="input-data"/>
            <input type="hidden" name="_method" value="PUT"/>
            <input type="submit" value="Pagar"/>
        </form>
    </div>
</div>


<div class="container">

<?php
function make_request_btn($numero) {
    global $webroot;
    return '<td class="table-buttons" style="font-size: 1.5em; padding: 10px 10px 0 10px;">'.
    "<form action='' method='post'>".
    '<input type="hidden" name="_method" value="PUT"/>'.
    '<input type="hidden" name="numero" value="'.htmlspecialchars($numero, ENT_QUOTES, 'UTF-8').'" />'.
    '<button type="submit" data-original-title="Pagar Reserva" data-placement="bottom" data-toggle="tooltip"'.
    'class="tooltipper" class="btn btn-xs btn-danger"><i class="glyphicon glyphicon-usd"></i></button>'.
    '</form></td>';
}
    draw_table($table, "Lista de Reservas", null, ["Numero", "Estado", "Accoes"], [null, null, 'make_request_btn'], [["numero"],["estado"],["numero"]], null);
?>

    

</div>
<?php
include $root . "template/foot.php";
?>