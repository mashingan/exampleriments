// source:
//https://oddblogger.com/daemon-threads-java-how-to-not-use-them/
//
//compile:
//javac DaemonThreadExample.java
//run on windows:
//java -cp .;libs DaemonThreadExample
//run on linux:
//java -cp .:libs DaemonThreadExample

class UserThread extends Thread {
    public void run() {
        System.out.println("User thread starting");
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println("User thread finishing");
    }
}

class DaemonThread extends Thread {
    public void run() {
        while (true) {
            System.out.println("Daemon thread processing");
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}

public class DaemonThreadExample {
    public static void main(String args[]) {
        Thread userthread = new UserThread();
        Thread daemonthread = new DaemonThread();
        daemonthread.setDaemon(true);

        userthread.start();
        daemonthread.start();
    }
}
