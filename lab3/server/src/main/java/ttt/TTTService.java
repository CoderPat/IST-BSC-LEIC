package ttt;
import java.rmi.Remote;
import java.rmi.RemoteException;

public interface TTTService extends Remote{
  int checkWinner() throws RemoteException;
  boolean play(int row, int column, int player) throws RemoteException;
  String currentBoard() throws RemoteException;
}
