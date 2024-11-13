import random
import matplotlib.pyplot as plt


# Преобразования для фрактала
def transformation1(point):
    x, y = point
    return (0, 0.16 * y)


def transformation2(point):
    x, y = point
    return (0.85 * x + 0.04 * y, -0.04 * x + 0.85 * y + 1.6)


def transformation3(point):
    x, y = point
    return (0.2 * x - 0.26 * y, 0.23 * x + 0.22 * y + 1.6)


def transformation4(point):
    x, y = point
    return (-0.15 * x + 0.28 * y, 0.26 * x + 0.24 * y + 0.44)


# Применение случайного преобразования
def choose_transformation(point, rand_value):
    if rand_value < 0.01:
        return transformation1(point)
    elif rand_value < 0.86:
        return transformation2(point)
    elif rand_value < 0.93:
        return transformation3(point)
    else:
        return transformation4(point)


# Генерация следующей точки фрактала
def generate_next_point(point, rand_value):
    return choose_transformation(point, rand_value)


# Рекурсивное построение фрактала
def build_fern(current_point, steps):
    points = []
    point = current_point
    for _ in range(steps):
        rand_value = random.random()  # Генерируем случайное число от 0 до 1
        point = generate_next_point(point, rand_value)
        points.append(point)
    return points


# Визуализация фрактала
def plot_fern(points):
    x_vals, y_vals = zip(*points)
    plt.figure(figsize=(6, 6))

    # Сделаем точки чуть больше
    plt.scatter(x_vals, y_vals, color="green", s=4, edgecolors="none")

    # Установим пределы осей для лучшего отображения
    plt.xlim(-3, 3)
    plt.ylim(0, 10)

    # Включаем оси
    plt.axis("on")

    # Добавляем больше меток на осях
    plt.xticks(range(-3, 4, 1))  # Метки на оси X от -3 до 3 с шагом 1
    plt.yticks(range(0, 11, 1))  # Метки на оси Y от 0 до 10 с шагом 1

    plt.show()


# Главная программа
if __name__ == "__main__":
    steps = 10000  # Количество шагов рекурсии (можно увеличить до 10000 или больше для лучшего результата)
    random.seed(42)  # Устанавливаем фиксированное зерно для воспроизводимости
    initial_point = (0, 0)
    fern_points = build_fern(initial_point, steps)
    plot_fern(fern_points)
