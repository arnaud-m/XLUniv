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
	    $group = htmlspecialchars($_POST['Group']); 
            if ($group == "autre") {
                $group = htmlspecialchars($_POST['which-Group']); 
            }		

            $team = htmlspecialchars($_POST['Teamname']); 

            $member1 = htmlspecialchars($_POST['Member1']); 
            $member2 = htmlspecialchars($_POST['Member2']); 
            
            $fd = fopen ("teamfile.csv", "a");
            if ($fd) {
                fwrite($fd, $group.";".$team.";".$member1.";".$member2."\r\n");
                fclose($fd);
            }
        }
    ?>
</html>