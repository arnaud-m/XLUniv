<!DOCTYPE html>
<html lang="fr">
    <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <link rel="stylesheet" href="fstyle.css">
        <title> Merci </title>
    </head>

    <body>
        <header>
            <center> <img src="Concoursprog.jpg" alt="concours programmation" height="100"/> </center>
        </header>

        <div class="overall">
            <section class="thanks" style="font-size:3em;">
                <p>
                    Merci votre inscription a été prise en compte !
                </p>
            </section>
        </div>

        <footer>
            © Lucas Guiglionia et Thomas Dissaux
        </footer>
    </body>

    <?php 
        if (isset($_POST['send'])) {

            $Group = "";
            if (isset($_POST['L1'])) {
                $Group = $Group."L1";
            } if (isset($_POST['L2'])) {
                $Group = $Group."L2";
            } if (isset($_POST['L3'])) {
                $Group = $Group."L3";
            } if (isset($_POST['M1'])) {
                $Group = $Group."M1";
            } if (isset($_POST['M2'])) {
                $Group = $Group."M2";
            }

            $Team = htmlspecialchars($_POST['Teamname']); 

            $Member1 = htmlspecialchars($_POST['Member1']); 
            $Member2 = htmlspecialchars($_POST['Member2']); 

            $suggest = htmlspecialchars($_POST['suggest']); 
            
            $fd = fopen ("teamfile.csv", "a");
            if ($fd) {
                fwrite($fd, $Group.";".$Team.";".$Member1.";".$Member2."\r\n");
                fclose($fd);
            }
        }
    ?>
</html>