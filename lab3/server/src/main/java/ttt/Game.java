package ttt;

import java.util.Scanner;
import java.rmi.*;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

public class Game {
	 public static void main(String args[]){
        System.out.println("Main OK");
        try{
            TTTService ttt = new TTT();
            System.out.println("Created TTT");
            Naming.rebind("TTTService", ttt);

            System.out.println("Tick Tack Toe server ready");
            System.out.println("<Press enter to shutdown>");

            System.in.read();
            System.exit(0);
        }catch(Exception e) {
            System.out.println("Tick Tack Toe server main " + e.getMessage());
        }
    }

}
