<nav class="navbar navbar-inverse navbar-fixed-top" role="navigation">
    <div class="container">
        <div class="navbar-header">
            <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
            <span class="sr-only">Toggle navigation</span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            </button>
            <a class="navbar-brand" href="<?=$webroot?>">Super Database</a>
        </div>
        <div id="navbar" class="navbar-collapse collapse">
            <ul class="nav navbar-nav navbar-right">
                <li class="dropdown">
                    <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false"><?= $title ?> <span class="caret"></span></a>
                    <ul class="dropdown-menu">
                        <li class="dropdown-header">Edifícios</li>
                        <li><a href="<?=$webroot?>/pages/edificio">Ver Edifícios</a></li>
                        <li role="separator" class="divider">Alugáveis</li>
                        <li class="dropdown-header">Alugáveis</li>
                        <li><a href="<?=$webroot?>/pages/espaco">Ver Espaços</a></li>
                        <li><a href="<?=$webroot?>/pages/posto">Ver Postos</a></li>
                        <li role="separator" class="divider">Ofertas</li>
                        <li class="dropdown-header">Ofertas</li>
                        <li><a href="<?=$webroot?>/pages/oferta">Ver Ofertas</a></li>
                        <li><a href="<?=$webroot?>/pages/reserva">Ver Reservas</a></li>
                    </ul>
                </li>
                
            </ul>
            
        </div><!--/.navbar-collapse -->
    </div>
</nav>
