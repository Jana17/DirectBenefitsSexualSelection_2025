///////Code for: Sexual selection driven by direct benefits leads to the erosion of direct benefits
///////Jana Riederer, j.m.riederer@rug.nl

#include <stdio.h>
#include <iostream>
#include <cmath>
#include <cfenv>
#include <climits>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

#include "rndutils.hpp"

using reng_type = rndutils::default_engine;
enum sex { male, female };

struct Param {

    const int max_time = 50001;
    const int save_interval = 1; 
    size_t used_seed;
    const size_t start_seed = 9;
    const size_t end_seed = 10; 

    const double initial_tau = 0.0;
    const double initial_p = 0.0; 
    const double initial_chi = 10.0; 

    const double mu = 0.01;
    const double mut_stdev_a = 0.05;

    const int max_pop_size = 1000;
    const int initial_nr_m = 500;
    const int initial_nr_f = 500;
    const double resource_min = 0.5;
    const double resource_max = 1.5;
    const double survival_scaling = 1.0;
    const int max_male_nr = 10;
    const int clutchsize_poisson = 5;
};

class rnd_j {
public:
    reng_type rndgen;

    rnd_j() {
        std::random_device rd;
        reng_type rndgen_t(rd());
        rndgen = rndgen_t;
    }

    rnd_j(const Param& P) {
        rndgen = reng_type(P.used_seed);
        set_mutate_prob(P.mu);
        set_resource_dist(P.resource_min, P.resource_max);
    }


    // true or false with 50/50 probability:
    bool flip_coin() {
        return coin_flip(rndgen);
    }

    bool bernouilli(double p) {
        return std::bernoulli_distribution(p)(rndgen);
    }

    int poisson(int o) {
        return std::poisson_distribution<int>(o)(rndgen);
    }

    double uniform_real(double min, double max) {
        return std::uniform_real_distribution<double>(min, max)(rndgen);
    }

    double uniform() {
        return unif(rndgen);
    }

    double normalDist(double m, double stdev) {
        return std::normal_distribution<double>(m, stdev) (rndgen);
    }

    void set_resource_dist(double min, double max) {
        resource_dist = std::uniform_real_distribution<double>(min, max);
    }

    double generate_resources() {
        return resource_dist(rndgen);
    }

    sex get_random_sex() {
        if (flip_coin()) {
            return female;
        }
        return male;
    }

    double mutate_bounded_trait_normalDist(const double& old_trait_value,
        const double& stdev_mut,
        const double& lowbound,
        const double& highbound) {
        if (mutate()) {
            auto new_trait_value = normalDist(old_trait_value, stdev_mut);
            while (new_trait_value < lowbound || new_trait_value > highbound) {
                new_trait_value = normalDist(old_trait_value, stdev_mut);
            }
            return new_trait_value;
        }
        return old_trait_value;
    }

    double mutate_unbounded_trait_normalDist(const double& old_trait_value, const double& stdev_mut) {
        if (mutate()) {
            auto new_trait_value = normalDist(old_trait_value, stdev_mut);
            return new_trait_value;
        }
        return old_trait_value;
    }

    // picks a random number in [0, n-1], useful when picking randomly from a vector.
    size_t random_number(size_t n) {
        if (n == 1) return 0;
        return std::uniform_int_distribution<>(0, static_cast<int>(n) - 1)(rndgen);
    }

    //// setters:
    void set_mutate_prob(double m) {
        mutate_prob = std::bernoulli_distribution(m);
    }

    bool mutate() {
        return mutate_prob(rndgen);
    }

private:
    std::bernoulli_distribution coin_flip = std::bernoulli_distribution(0.5); // we can keep this fixed.
    std::uniform_real_distribution<double> unif = std::uniform_real_distribution<double>(0, 1.0);

    std::bernoulli_distribution mutate_prob;
    std::uniform_real_distribution<double> resource_dist;
};


struct Individual {

    sex S;

    double tau; //this is the investment strategy of the male (re-named S in the methods section)
    double p;
    double chi; //corresponds to 1/nu
    double t;
    double gamma;
    double Res;

    //Keeping track
    int live_offspring_count;
    int dead_offspring_count;
    int HasMated; //0=yes, 1=no
    //etc.

    // default constructor with default values (e.g. individuals not the result of mating):
    Individual(const Param& parameters,
        sex initial_sex, rnd_j& rnd) : S(initial_sex) {
        tau = parameters.initial_tau;
        p = parameters.initial_p;
        chi = parameters.initial_chi;
        Res = rnd.uniform_real(parameters.resource_min, parameters.resource_max);
        t = tau * Res;
        gamma = Res - t;

        //Keeping track
        live_offspring_count = 0;
        dead_offspring_count = 0;
        HasMated = 1; //0=yes, 1=no
    }

    // constructor, creating Individual based on a mating event
    Individual(const Individual& parent1,
        const Individual& parent2,
        const Param& P,
        sex initial_sex,
        rnd_j& rnd) : S(initial_sex) {

        if (rnd.flip_coin()) {
            p = parent1.p;
        }
        else {
            p = parent2.p;
        }
        chi = P.initial_chi;
        tau = rnd.flip_coin() ? parent1.tau : parent2.tau;

        p = rnd.mutate_unbounded_trait_normalDist(p, P.mut_stdev_a);
        tau = rnd.mutate_bounded_trait_normalDist(tau, P.mut_stdev_a, 0, 1);

        Res = rnd.generate_resources();
        t = tau * Res;
        gamma = Res - t;

        //Keeping track
        live_offspring_count = 0;
        dead_offspring_count = 0;
        HasMated = 1; //0=yes, 1=no
    }
};

void add_individuals(std::vector<Individual>& v,
    size_t num_individuals,
    sex focal_sex,
    rnd_j& rng,
    const Param& parameters) {
    for (size_t i = 0; i < num_individuals; i++) {
        v.push_back(Individual(parameters, focal_sex, rng));
    }
}

void initialise(const Param& parameters,
    std::vector<Individual>& males,
    std::vector<Individual>& females,
    rnd_j& rng)
{
    rng = rnd_j(parameters);
    males.clear();
    females.clear();
    add_individuals(males, parameters.initial_nr_m, sex::male, rng, parameters);
    add_individuals(females, parameters.initial_nr_f, sex::female, rng, parameters);
}

void find_mate(size_t& mother_id,
    size_t& father_id,
    bool& has_mated,
    const Param& parameters,
    std::vector<Individual>& pop_females,
    std::vector<Individual>& pop_males,
    rnd_j& rng) {

    has_mated = false;
    auto chi_sq = pop_females[mother_id].chi * pop_females[mother_id].chi;
    int male_nr = 1;

    while (true) {
        father_id = rng.random_number(pop_males.size());
        double difference_p_t = pop_females[mother_id].p - pop_males[father_id].t;
        double prob_mating = exp(-0.5 * difference_p_t * difference_p_t * chi_sq);
        if (rng.bernouilli(prob_mating)) {
            has_mated = true;
            pop_males[father_id].HasMated = 0;
            pop_females[mother_id].HasMated = 0;
            break;
        }
        if (male_nr == parameters.max_male_nr) {
            has_mated = true;
            pop_males[father_id].HasMated = 0;
            pop_females[mother_id].HasMated = 0;
            break;
        }
        male_nr = male_nr + 1;
    }
}

void reproduce(const size_t& mother_id,
    const size_t& father_id,
    const Param& parameters,
    rnd_j& rng,
    std::vector<Individual>& pop_females,
    std::vector<Individual>& pop_males,
    std::vector<Individual>& new_females,
    std::vector<Individual>& new_males,
    int surviving_clutch) {

    for (int offspring_nr = 0; offspring_nr < surviving_clutch; offspring_nr++) {
        sex random_sex = rng.get_random_sex();
        if (random_sex == female) {
            new_females.push_back(Individual(pop_females[mother_id],
                pop_males[father_id],
                parameters,
                random_sex,
                rng));
        }
        else {
            new_males.push_back(Individual(pop_females[mother_id],
                pop_males[father_id],
                parameters,
                random_sex,
                rng));
        }
    }
}


int main() {
    std::cout << "this is running" << std::endl;
    Param parameters;

    std::ofstream ofs1("Filename.csv"); //Edit this to be the desired filename
    ofs1 << "Time" << "," << "Rep" << "," << "PopSizeF" << "," << "PopSizeM" << "," << "PopSize" << ","
        << "avg_tau" << "," << "avg_p" << "," << "avg_chi" << "," << "avg_t" << "," << "avg_gamma" << ","
        << "avg_surviving_offspring_per_mother" << "," << "avg_dead_offspring_per_mother" << ","
        << "avg_Res";
    ofs1 << '\n';

    std::vector<Individual> pop_males;
    std::vector<Individual> pop_females;
    std::vector<Individual> new_males;
    std::vector<Individual> new_females;

    rnd_j rng;

    for (auto current_seed = parameters.start_seed; current_seed < parameters.end_seed; ++current_seed) {

        std::cout << "Seed: " << current_seed << std::endl;
        parameters.used_seed = current_seed;
        initialise(parameters, pop_males, pop_females, rng);

        for (int time = 0; time < parameters.max_time; time++) {
            
            new_males.clear();
            new_females.clear();

            for (int mating_count_nr = 0; mating_count_nr < pop_females.size(); mating_count_nr++) {

                size_t father_id;
                size_t mother_id = mating_count_nr;
                bool has_mated;

                find_mate(mother_id, father_id, has_mated, parameters, pop_females, pop_males, rng);

                if (has_mated) {
                    int clutch_size = rng.poisson(parameters.clutchsize_poisson);
                    int surviving_clutch=0;
                    double res_off = pop_males[father_id].gamma;
                    double offspring_survival_prob = 1.0 - exp(-parameters.survival_scaling * res_off);
                    for (int egg_nr = 0; egg_nr < clutch_size; egg_nr++) {
                            if (rng.bernouilli(offspring_survival_prob)) {
                                surviving_clutch = surviving_clutch + 1;
                                pop_females[mother_id].live_offspring_count++;
                                pop_males[father_id].live_offspring_count++;
                            }
                            else {
                                pop_females[mother_id].dead_offspring_count++;
                                pop_males[father_id].dead_offspring_count++;
                            }
                    }
                    reproduce(mother_id, father_id, parameters, rng,
                        pop_females, pop_males, new_females, new_males, surviving_clutch); 
                }

            }

            while ((new_females.size() + new_males.size()) > parameters.max_pop_size) {
                int current_pop_size = new_females.size() + new_males.size();
                int dead_indiv = rng.random_number(current_pop_size);
                if (dead_indiv < new_females.size()) {
                    new_females.erase(new_females.begin() + dead_indiv);
                }
                else {
                    dead_indiv = dead_indiv - new_females.size();
                    new_males.erase(new_males.begin() + dead_indiv);
                }
            }

            if (time % 5000 == 0) { std::cout << "time: " << time << std::endl; }

            // save data to file:
            if (time % parameters.save_interval == 0) {
                double avg_tau = 0;
                double avg_t = 0;
                double avg_p = 0;
                double avg_chi = 0;
                double avg_gamma = 0;
                double avg_surviving_off_f = 0;
                double avg_dead_off_f = 0;
                double avg_Res = 0;

                for (const auto& i : pop_females) {
                    avg_tau += i.tau;
                    avg_p += i.p;
                    avg_chi += i.chi;
                    avg_surviving_off_f += i.live_offspring_count;
                    avg_dead_off_f += i.dead_offspring_count;
                    avg_Res += i.Res;
                }
                for (const auto& i : pop_males) {
                    avg_tau += i.tau;
                    avg_p += i.p;
                    avg_chi += i.chi;
                    avg_t += i.t;
                    avg_gamma += i.gamma;
                    avg_Res += i.Res;
                }

                double popsize_mult = 1.0 / (static_cast<double>(pop_females.size()) + static_cast<double>(pop_males.size()));
                double popsize_m_mult = 1.0 / (static_cast<double>(pop_males.size()));
                double popsize_f_mult = 1.0 / (static_cast<double>(pop_females.size()));
                avg_tau *= popsize_mult;
                avg_p *= popsize_mult;
                avg_chi *= popsize_mult;
                avg_t = avg_t * popsize_m_mult;
                avg_gamma = avg_gamma * popsize_m_mult;
                avg_surviving_off_f = avg_surviving_off_f * popsize_f_mult;
                avg_dead_off_f = avg_dead_off_f * popsize_f_mult;
                avg_Res = avg_Res * popsize_mult;

                ofs1 << time << "," << current_seed << "," << pop_females.size() << "," << pop_males.size() << ","
                    << pop_females.size() + pop_males.size() << ","
                    << avg_tau << "," << avg_p << "," << avg_chi << ","
                    << avg_t << "," << avg_gamma << "," << avg_surviving_off_f << "," << avg_dead_off_f << ","
                    << avg_Res
                    << '\n';
            }

            std::swap(pop_females, new_females);
            std::swap(pop_males, new_males);

            //how to handle extinctions
            if (pop_females.size() < 1) {
                //population cannot continue
                time = parameters.max_time;
            }
            if (pop_males.size() < 1) {
                //population cannot continue
                time = parameters.max_time;
            }

        }

    }
    ofs1.close();
    return 0;
}
