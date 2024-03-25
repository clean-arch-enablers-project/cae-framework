package com.cae.trier;

/**
 * Actions that don't have either input or output
 */
public class RunnableAction implements Action<Void, Void> {

    /**
     * Action itself
     */
    private final Runnable runnable;

    public RunnableAction(Runnable runnable) {
        this.runnable = runnable;
    }

    /**
     * Where the action itself gets executed
     * @param input null as runnables don't consume anything as
     *              input
     * @return null as runnables don't return anything
     */
    @Override
    public Void execute(Void input) {
        this.runnable.run();
        return null;
    }
}
