<?php
require_once "../../func/init.php";
include $root . "model/posto.php";
include $root . "view/table.php";
$title = "Listar Postos";

include $root . "template/head.php"; 
include $root . "template/navbar.php";
$table = posto_getall($db);
?>


<link rel="stylesheet" type="text/css" href="<?= $webroot ?>/assets/css/tabler.css">

 <div class="jumbotron">
    <div class="container">  
        <h3>
            Criar Posto
        </h3>
        <form class="betterform" action=<?= $webroot . "/api/posto.php" . "?callback=" . $webroot . "/pages/posto/"?> method="post">
            <label for="input-morada">Morada do edificio em que esta inserido:</label>
            <input type="text" name="morada" id="input-morada"/>
            <label for="input-codigo_espaco">Codigo do Espaco em que esta inserido:</label>
            <input type="text" name="codigo_espaco" id="input-codigo_espaco"/>
            <label for="input-codigo">Codigo do Posto:</label>
            <input type="text" name="codigo" id="input-codigo"/>
            <label for="input-foto">Link da Foto:</label>
            <input type="text" name="foto" id="input-foto"/>
            <label for="input-nif">NIF do Dono:</label>
            <input type="text" name="nif" id="input-nif" value="123456719"/>
            <input type="submit" value="Criar"/>
        </form>
    </div>
</div>



<div class="container">

<?php
function make_request_btn($morada, $codigo) {
    global $webroot;
    return  '<td class="table-buttons" style="font-size: 1.5em; padding: 10px 10px 0 10px;">'.
    "<form action='$webroot/pages/oferta/index.php' method='POST'>".
    '<input type="hidden" name="_method" value="DELETE" />'.
    '<input type="hidden" name="morada" value="'.htmlspecialchars($morada, ENT_QUOTES, 'UTF-8').'" />'.
    '<input type="hidden" name="codigo" value="'.htmlspecialchars($codigo, ENT_QUOTES, 'UTF-8').'" />'.
    '<button type="submit" data-original-title="Criar Oferta" data-placement="bottom" data-toggle="tooltip"'.
    'class="tooltipper" class="btn btn-xs btn-danger"><i class="glyphicon glyphicon-folder-close"></i></button>'.
    '</form>' .
    "<form action='$webroot/api/posto.php?callback=".urlencode("$webroot/pages/posto")."' method='POST'>".
    '<input type="hidden" name="_method" value="DELETE" />'.
    '<input type="hidden" name="morada" value="'.htmlspecialchars($morada, ENT_QUOTES, 'UTF-8').'" />'.
    '<input type="hidden" name="codigo" value="'.htmlspecialchars($codigo, ENT_QUOTES, 'UTF-8').'" />'.
    '<button type="submit" data-original-title="Apagar Posto" data-placement="bottom" data-toggle="tooltip"'.
    'class="tooltipper" class="btn btn-xs btn-danger"><i class="glyphicon glyphicon-trash"></i></button>'.
    '</form></td>';
}
    draw_table($table, "Lista de Postos", null, ["Morada", "Codigo do Espaco", "Codigo do Posto", "Accoes"], [null, null, null, 'make_request_btn'], [["morada"], ["codigo_espaco"], ["codigo"], ["morada", "codigo"]], null);
?>

    

</div>
<?php
include $root . "template/foot.php";
?>