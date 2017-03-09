import java.rmi.Remote;

public interface TTTService extends Remote{
  int checkWinner();
  boolean play(int row, int column, int player);
  String currentBoard();
}
