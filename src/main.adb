with Algorithm; use Algorithm;
with Botstate; use Botstate;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;


procedure Main is
    procedure Usage
    is
    begin
        Put_Line ("./adaroombot <name of tty>");
    end Usage;

    My_Bot : Bot;
begin
    if Argument_Count = 1 then
        My_Bot.Init (TTY_Name  => Argument(1),
                     Algo_Type => Pong);
        My_Bot.Start;
        My_Bot.Kill;
    else
        Usage;
        return;
    end if;

end Main;
