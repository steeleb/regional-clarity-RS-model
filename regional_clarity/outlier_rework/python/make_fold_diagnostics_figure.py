"""Per-fold train/val RMSE figure - the overfitting diagnostic."""
import matplotlib.pyplot as plt
import pandas as pd

OUT_DIR = "results"
FIG_DIR = "results/figures"
MODELS = ["xgboost", "lightgbm", "nn"]
MODEL_COLORS = {"xgboost": "#1b9e77", "lightgbm": "#d95f02", "nn": "#7570b3"}


def main():
    dfs = [pd.read_csv(f"{OUT_DIR}/fold_diagnostics_{m}.csv") for m in MODELS]
    all_df = pd.concat(dfs, ignore_index=True)

    fig, axes = plt.subplots(1, 3, figsize=(14, 4.3))
    for ax, name in zip(axes, MODELS):
        sub = all_df[all_df["model"] == name].sort_values("held_out_part")
        x = sub["held_out_part"].values
        ax.bar(x - 0.18, sub["train_rmse"], width=0.36, color="#B0B8BD", label="train")
        ax.bar(x + 0.18, sub["val_rmse"], width=0.36, color=MODEL_COLORS[name], label="held-out val")
        ax.set_xticks(x)
        ax.set_xlabel("held-out partition")
        ax.set_title(name)
        ax.axhline(sub["val_rmse"].mean(), color="black", lw=0.8, linestyle=":")
    axes[0].set_ylabel("RMSE (m)")
    axes[0].legend(fontsize=9)
    fig.suptitle("Per-fold train vs. held-out validation RMSE (dotted line = mean val RMSE)")
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/fold_diagnostics.png", dpi=150)
    plt.close(fig)
    print("wrote", f"{FIG_DIR}/fold_diagnostics.png")


if __name__ == "__main__":
    main()
