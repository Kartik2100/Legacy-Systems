--Assignment 2
--Name: Kartik Patel
--Student Number: 1052085
--Course: CIS*3190

--Packages needed
with ada.Text_IO; use Ada.Text_IO;
with ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with ada.strings.unbounded; use ada.strings.unbounded;
with ada.strings.unbounded.Text_IO; use ada.strings.unbounded.Text_IO;
with ada.numerics.discrete_random;
 
--The main hangman program
procedure hangman is
    
    --This is a function that changes strings to unbounded strings
    function uString(Source : String) return unbounded_string renames 
    ada.strings.unbounded.to_unbounded_string;

    --initializing all the variables
    p : array(1..12, 1..12) of character;
    d : array(1..20) of character;
    n : array(1..26) of character;
    a : unbounded_string;
    b : unbounded_string;
    guess, ans : character;
    
    u : array(1..50) of integer;
    m, i, j, k1, k2, k3, k4, w, t1, r, l, c, rnd, num1, num2, num3, k5, k6: integer;

    temp : unbounded_string;
    type dict is array(1..50) of unbounded_string;
    type randomNum is new Integer range 1..50;

    --This is the package used to initialize the method of randomizing the words picked later on
    package Rand_Int is new ada.numerics.discrete_random(randomNum);
    use Rand_Int;
    gen : Generator;
    number : randomNum;

    --The list of words availiable in the game of hangman.
    words : dict := (uString("gum"),uString("sin"),uString("for"),uString("cry"),
    uString("lug"),uString("bye"),uString("fly"),uString("ugly"),uString("each"),
    uString("from"),uString("work"),uString("talk"),uString("with"),uString("self"),
    uString("pizza"),uString("thing"),uString("feign"),uString("fiend"),uString("elbow"),
    uString("fault"),uString("dirty"),uString("budget"),uString("spirit"),uString("quaint"),
    uString("maiden"),uString("escort"),uString("pickax"),uString("example"),uString("tension"),
    uString("quinine"),uString("kidney"),uString("replica"),uString("sleeper"),uString("triangle"),
    uString("kangaroo"),uString("mahogany"),uString("sergeant"),uString("sequence"),
    uString("moustache"),uString("dangerous"),uString("scientist"),uString("different"),
    uString("quiescent"),uString("magistrate"),uString("erroneously"),uString("loudspeaker"),
    uString("phytotoxic"),uString("matrimonial"),uString("parasympathomimetic"),uString("thigmotropism"));
--Starts the main procedure.
begin
    k5 := 1;
    put_line("THE GAME OF HANGMAN");
    --Until the user does not want to play anymore, the game will keep looping.
    while (k5 = 1) loop
      --Here, the arrays are filled with starting values and intialized in case if the game restarts.
      for i in 1..12 loop
         for j in 1..12 loop
            p(i,j) := ' ';
         end loop;
      end loop;
      
      for i in 1..20 loop
         d(i) := '-';
      end loop;

      for i in 1..26 loop
         n(i) := ' ';
      end loop;

      for i in 1..50 loop
         u(i) := 0;
      end loop;

      for i in 1..12 loop
         p(i,1) := 'X';
      end loop;
      
      for j in 1..7 loop
         p(1,j) := 'X';
      end loop;

      p(2,7) := 'X';
      c := 1;
      w := 50;
      m := 0;
      k6 := 0;
      num1 := 0;
      num2:= 0;

      if (c < w) then
         --This is the method used to randomized which word the user will get.
         --It is needed to make the game less predictable.
         reset(gen);
         while (num1 = 0) loop
            number := random(gen);
            temp := uString(randomNum'Image(number));
            rnd := integer'Value(To_String(temp));
            if ((u(rnd) - 1) /= 0) then
               num1 := 1;
            end if;
         end loop;
         num1 := 0;
         if ((u(rnd) - 1) /= 0) then
            u(rnd) := 1;
            c := c + 1;
            t1 := 0;
            a := words(rnd);
            l := length(a);
            --The User will recieve a notice of how many letters there are in the word.
            for i in 1..l loop
               put(d(i));
            end loop;    
            put_line("");
         end if;
         for k1 in 1..26 loop
            --The letters that the user already guess will be printed here.
            put_line("Here are the letters you used: ");
            if(k1 /= 1) then
               for k4 in 1..(k1-1) loop
                  put(n(k4));
                  put(",");
               end loop;
            end if;
            if (n(k1) = ' ') then
               --The user will be asked to guess the letter with the code below.
               put_line(" ");
               put_line("What is your guess? ");
               r := 0;
               get(guess);
               --In the while loop below, it will check if the user used a letter before.
               --If the letter is used before, the user will be asked to choose another letter.
               --They will be reminded what letter was used before and will kept being asked to
               --choose another letter until they choose a letter they never picked before.
               while (num2 = 0)loop
                  for k2 in 1..26 loop
                     if(n(k2) = guess) then
                        put_line("You guessed that letter before");
                        num3 := 1;
                     end if;
                  end loop;
                  if(num3 = 1) then
                     put_line("Here are the letters you used: ");
                     if(k1 /= 1) then
                        for k4 in 1..(k1-1) loop
                           put(n(k4));
                           put(",");
                        end loop;
                     end if;
                     put_line(" ");
                     put_line("What is your guess? ");
                     r := 0;
                     get(guess);
                     num3 :=0;
                  else
                     --the while loop breaks here when a letter was chose that was not before.
                     num2 :=1;
                  end if;
               end loop;
               num2 := 0;
               num3 := 0;
               n(k1) := guess;
               t1 := t1 + 1;
               --checks to see if the letter guessed is in the word.
               --if found, it replaces the - with the letter.
               for k3 in 1..l loop
                  if(element(a,k3) = guess) then
                     d(k3) := guess;
                     r:=r+1;
                  end if;
               end loop;
               if (r = 0) then
                  m := m + 1;
                  --draws a part of the hangman after each incorrect guess
                  put_line("Sorry, that letter isn't in the word.");
                  --first wrong guess.
                  if (m = 1) then
                     put_line("First we draw a head.");
                     p(3,6) := '-'; 
                     p(3,7) := '-'; 
                     p(3,8) := '-'; 
                     p(4,5) := '(';
                     p(4,6) := '.';
                     p(4,8) := '.';
                     p(4,9) := ')'; 
                     p(5,6) := '-'; 
                     p(5,7) := '-';
                     p(5,8) := '-';
                  --second wrong guess.
                  elsif (m = 2) then
                     put_line("Now we draw a body.");
                     for i in 6..9 loop
                        p(i,7) := 'X';
                     end loop;
                  --third wrong guess.
                  elsif (m = 3) then
                     put_line("Next we draw an arm.");
                     for i in 4..7 loop
                        p(i, i - 1) := '\';
                     end loop;
                  --forth wrong guess.
                  elsif (m = 4) then
                     put_line("This time it's the other arm.");
                     p(4,11) := '/';
                     p(5,10) := '/';
                     p(6,9) := '/';
                     p(7,8) := '/';
                  --fifth wrong guess.
                  elsif (m = 5) then
                     put_line("Now, let's draw the right leg.");
                     p(10,6) := '/';
                     p(11,5) := '/';
                  --sixth wrong guess.
                  elsif (m = 6) then
                     put_line("This time we draw the left leg.");
                     p(10,8) := '\'; 
                     p(11,9) := '\';
                  --seventh wrong guess.
                  elsif (m = 7) then
                     put_line("Now we put up a hand.");
                     p(3,11) := '\';
                  --eighth wrong guess.
                  elsif (m = 8) then
                     put_line("Next the other hand.");
                     p(3,3) := '/';
                  --ninth wrong guess.
                  elsif (m = 9) then
                     put_line("Now we draw one foot.");
                     p(12,10) := '\'; 
                     p(12,11) := '-';
                  --tenth and last wrong guess.
                  elsif (m = 10) then
                     put_line("Here's the other foot -- You're hung!!.");
                     p(12,3) := '-';
                     p(12,4) := '/';
                  end if;
                  --prints out the current hangman picture.
                  for i in 1..12 loop
                     for j in 1..12 loop
                        put(p(i,j));
                     end loop;
                     put_line("");
                  end loop;
                  --Checks if the person lost to maxing out guess.
                  if ((m - 10) = 0) then
                     put("Sorry, you loose. The word was ");
                     put_line(a);
                     put_line("You missed that one.");
                     put_line("Do you want another word? (Y/N) ");
                     get(ans);
                     --user presses Y to play again or else the program ends.
                     if (ans = 'Y') then
                        k5 := 1;
                        exit;
                     else
                        put_line("It's been fun! Bye for now.");
                        put_line("Ending...");
                        k5 := 0;
                        exit;
                     end if;
                  end if;
               else
                  k6 := 0;
                  for i in 1..l loop
                     if (d(i) = '-') then
                        for i in 1..l loop
                           put(d(i));
                        end loop;    
                        put_line("");
                        exit;
                     end if;
                     k6:= k6 + 1;
                  end loop;
                  --checks if user won the game by guessing only letter (not guessing the word).
                  if (k6 = l) then
                     put_line("You found the word.");
                     put_line("Do you want another word? (Y/N) ");
                     get(ans);
                     --user presses Y to play again or else the program ends.
                     if (ans = 'Y') then
                        k5 := 1;
                        exit;
                     else
                        put_line("It's been fun! Bye for now.");
                        put_line("Ending...");
                        k5 := 0;
                        exit;
                     end if;
                  end if;
                  --User makes the guess of the word.
                  put_line("What is your guess for the word? ");
                  get_line(b);
                  get_line(b);
                  --checks if user won the game by guessing the full word.
                  if (a = b) then
                     put("Right! It took you ");
                     put(t1);
                     put_line(" guesses");
                     --user presses Y to play again or else the program ends.
                     put_line("Do you want another word? (Y/N) ");
                     get(ans);
                     if (ans = 'Y') then
                        k5 := 1;
                        exit;
                     else
                        put_line("It's been fun! Bye for now.");
                        put_line("Ending...");
                        k5 := 0;
                        exit;
                     end if;
                  end if;
               end if;
            end if;
         end loop;
      else
         --if all the words are guessed.
         put_line("You did all the words");
         put_line("Ending...");
      end if; 
   end loop;
end hangman;