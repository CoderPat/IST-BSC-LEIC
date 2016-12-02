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
        <form class="betterform" action=<?= $webroot . "/api/posto.php" . "?redirect=" . $webroot . "pages/posto/"?> method="post">
            <label for="input-morada">Morada do edificio em que esta inserido:</label>
            <input type="text" name="morada" id="input-morada"/>
            <label for="input-codigo_espaco">Codigo do Espaco em que esta inserido:</label>
            <input type="text" name="codigo" id="input-codigo_espaco"/>
            <label for="input-codigo">Codigo do Posto:</label>
            <input type="text" name="codigo" id="input-codigo"/>
            <label for="input-foto">Link da Foto:</label>
            <input type="text" name="foto" id="input-foto"/>
            <input type="submit" value="Submit"/>
        </form>
    </div>
</div>



<div class="container">

<?php
function make_request_btn($morada, $codigo) {
    global $webroot;
    return  '<td class="table-buttons" style="font-size: 1.5em; padding: 10px 10px 0 10px;">'.
    "<form action='$webroot/api/posto.php?callback=".urlencode("$webroot/pages/posto")."' method='POST'>".
    '<input type="hidden" name="_method" value="DELETE" />'.
    '<input type="hidden" name="morada" value="'.htmlspecialchars($morada, ENT_QUOTES, 'UTF-8').'" />'.
    '<input type="hidden" name="codigo" value="'.htmlspecialchars($codigo, ENT_QUOTES, 'UTF-8').'" />'.
    '<button type="submit" data-original-title="Apagar Posto" data-placement="bottom" data-toggle="tooltip"'.
    'class="tooltipper" class="btn btn-xs btn-danger"><i class="glyphicon glyphicon-trash"></i></button>'.
    '</form></td>';
}
    draw_table($table, "Lista de Postos", null, ["Morada", "Codigo do Espaco", "Codigo do Posto", "Accoes"], [null, null, null, 'make_request_btn'], [["morada"], ["codigo"], ["codigo_espaco"], ["morada", "codigo"]], null);
?>

    

</div>
<?php
include $root . "template/foot.php";
?>