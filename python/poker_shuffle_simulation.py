import numpy as np
import pandas as pd


NUM_CARDS_PER_DECK = 52
NUM_SHUFFLES = 12
NUM_SIMULATIONS = 10000


class PokerShuffleSimulator(object):
    def __init__(self, n_cards_per_deck=NUM_CARDS_PER_DECK, n_shuffles=NUM_SHUFFLES, n_simulations=NUM_SIMULATIONS, method='random_riffle'):
        self.n_cards_per_deck = n_cards_per_deck
        self.n_shuffles = n_shuffles
        self.n_simulations = n_simulations
        self.method = method

        self.x0 = range(1, 1 + self.n_cards_per_deck)
        self.sim_deck_list = None
        self.randomness_error = None

    @staticmethod
    def tv_norm(x1, x2):
        return sum(abs(x1 - x2))

    @staticmethod
    def _gen_shuffle_index(n, p=0.5):
        index_0 = []
        index_1 = []
        for i in enumerate(np.random.binomial(1, p, size=n), 1):
            index_0.append(i[0]) if i[1] == 0 else index_1.append(i[0])
        return index_0, index_1

    def map_shuffle_type(self, method='random_riffle'):
        shuffle_type_mappting = {
            'bottom_up': self.bottom_up,
            'complete_riffle': self.complete_riffle,
            'random_riffle': self.random_riffle,
        }
        return shuffle_type_mappting[method]

    def bottom_up(self, deck):
        deck = list(deck)
        n = len(deck)
        n_cards = np.random.randint(n * 0.25, n * 0.75)
        shuffled = list(deck[n_cards:]) + list(deck[:n_cards])
        return shuffled

    def complete_riffle(self, deck):
        deck = list(deck)
        n = len(deck)
        x_l, x_r = deck[:n/2], deck[n/2:]
        shuffled = [None] * n
        if np.random.uniform() > 0.5:
            shuffled[::2], shuffled[1::2] = x_l, x_r
        else:
            shuffled[::2], shuffled[1::2] = x_r, x_l
        return shuffled

    def random_riffle(self, deck):
        n = len(deck)
        index_0, index_1 = self._gen_shuffle_index(n)
        shuffled_tuple = zip(index_0, deck[:len(index_0)]) + zip(index_1, deck[len(index_0):])
        shuffled = [i[1] for i in sorted(shuffled_tuple)]
        return shuffled

    def simulate_shuffle(self):
        self.sim_deck_list = []

        for k in range(self.n_simulations):
            df_deck = pd.DataFrame({'x0': self.x0}, index=range(1, 1 + self.n_cards_per_deck))

            for t in range(1, self.n_shuffles + 1):
                col_name_cur, col_name_pre = 'x' + str(t), 'x' + str(t - 1)
                shuffle_type = self.map_shuffle_type(self.method)
                df_deck[col_name_cur] = shuffle_type(df_deck[col_name_pre])

            self.sim_deck_list.append(df_deck)

    def evaluate_simulation(self):
        assert self.sim_deck_list is not None, 'please simulate shuffle.'
        self.randomness_error = {}
        for t in range(1, 1 + self.n_shuffles):
            col_name = 'x' + str(t)
            shuffled_t = pd.DataFrame([i[col_name] for i in self.sim_deck_list])

            tv_norms_t = []
            for i in range(1, 1 + self.n_cards_per_deck):
                hist_i_t = shuffled_t.loc[:, i].value_counts(normalize=True).values
                hist_unif = 1. / self.n_cards_per_deck
                tv_norm_i_t = self.tv_norm(hist_i_t, hist_unif)
                tv_norms_t.append(tv_norm_i_t)

            self.randomness_error[t] = tv_norms_t

    @property
    def eval_summary(self):
        assert self.randomness_error is not None, 'please evaluate simulation.'
        df_m = pd.DataFrame(self.randomness_error)
        return df_m.apply(np.mean).values, df_m.apply(np.std).values
