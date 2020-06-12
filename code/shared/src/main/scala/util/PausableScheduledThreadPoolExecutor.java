package scalaam.util;

/*
 *  A thread pool with the ability to pause the threadpool 
 *  Source: https://stackoverflow.com/questions/9748710/how-to-pause-resume-all-threads-in-an-executorservice-in-java (after some cherry-picking & modifications) 
 */

import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class PausableScheduledThreadPoolExecutor extends ScheduledThreadPoolExecutor {      
  public boolean isPaused = true;
  private ReentrantLock pauseLock = new ReentrantLock();
  private Condition unpaused = pauseLock.newCondition();
  /**
   * Default constructor for a simple fixed threadpool
   */
  public PausableScheduledThreadPoolExecutor(int corePoolSize) {
    super(corePoolSize);
  }

  /**
   * Executed before a task is assigned to a thread.
   */
  @Override
  protected void beforeExecute(Thread t, Runnable r) {
    pauseLock.lock();
    try {
      while (isPaused)
        unpaused.await();
    } catch (InterruptedException ie) {
      t.interrupt();
    } finally {
      pauseLock.unlock();
    }
    super.beforeExecute(t, r);
  }

  /**
   * Pause the threadpool. Running tasks will continue running, but new tasks
   * will not start untill the threadpool is resumed.
   */
  public void pause() {
    pauseLock.lock();
    try {
      isPaused = true;
    } finally {
      pauseLock.unlock();
    }
  }

  /**
   * Resume the threadpool.
   */
  public void resume() {
    pauseLock.lock();
    try {
      isPaused = false;
      unpaused.signalAll();
    } finally {
      pauseLock.unlock();
    }
  }

}